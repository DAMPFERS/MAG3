"""
АПИМ-24, Разуваев В.В.
SLAM
Лабораторная работа №1: генерация комнаты (прямоугольник с скошенным углом), симуляция двух 2D лазерных сканов
и реализация двух методов скан-матчинга:
1) scan-to-map brute-force — корреляция точек скана с occupancy-grid картой
2) scan-to-scan ICP (iterative closest point) — уточняющая оптимизация
    
"""

import numpy as np
import matplotlib.pyplot as plt
from math import cos, sin, atan2, pi



import utils_poses_and_points
import geometry_room
import simulation_laser
import occupancy_grid
import brute_force
import icp


from typing import List, Tuple
Point = Tuple[float, float]
Polygon = List[Point]
Edge = Tuple[Point, Point]


def pointInPoly(pt: Point, poly: Polygon) -> bool:
    """
    Проверка точки внутри полигона

    Args:
        pt (Point): координаты точки (x, y)
        poly (Polygon): Список вершин полигона

    Returns:
        bool: 1 если точка принадлежит полигону
    """
    x, y = pt
    inside = False
    n = len(poly)
    for i in range(n):
        xi, yi = poly[i]
        xj, yj = poly[(i + 1) % n]
        intersect = ((yi > y) != (yj > y)) and (x < (xj - xi)*(y - yi) / (yj - yi + 1e-12) + xi)
        if intersect:   inside = not inside
    return inside



def main():
    np.random.seed(0)
    w, h, cut = 6.2, 4.6, 1.0
    polygon = geometry_room.makeSkewedRectangle(w, h, cut)
    
    
    def sampleInPoly(poly: Polygon) -> Point:
        # выбор случайной позиции внутри полигона
        min_xy = poly.min(axis=0)
        max_xy = poly.max(axis=0)
        for _ in range(10000):
            x = np.random.uniform(min_xy[0] + 0.2, max_xy[0] - 0.2)
            y = np.random.uniform(min_xy[1] + 0.2, max_xy[1] - 0.2)
            if pointInPoly((x,y), poly):
                return np.array([x, y])
        raise RuntimeError("Не удалось выбрать точку внутри полигона")
    
    def sampleInPolyNearPoint(poly: Polygon, center: Point, radius=2.0) -> Point:
        """
        Выбор случайной позиции внутри полигона на расстоянии не больше radius от центральной точки

        Args:
            poly (Polygon): Полигон
            center (Point): Центральная точка
            radius (float): Максимальное расстояние от центра. Defaults to 2.0.

        Returns:
            Point: Случайная точка внутри полигона в пределах radius от center
        """
        for _ in range(1000):
            angle = np.random.uniform(0, 2 * pi)
            distance = np.random.uniform(0, radius)
            
            x = center[0] + distance * cos(angle)
            y = center[1] + distance * sin(angle)
            if pointInPoly((x, y), poly): return np.array([x, y])
        raise RuntimeError("Не удалось выбрать точку внутри полигона")
    
    
    
    pos1 = sampleInPoly(polygon)    # Начальная позиция
    th1 = np.random.uniform(-pi, pi)    # Начальный угол поворота
    # pos2 = sampleInPoly(polygon)    # Вторая позиция
    pos2 = sampleInPolyNearPoint(polygon, center=pos1, radius=2.0)    # Вторая позиция
    th2 = np.random.uniform(-pi, pi)    # Конечный угол поворота
    
    pose1 = np.array([pos1[0], pos1[1], th1])
    pose2 = np.array([pos2[0], pos2[1], th2])
    
    print(f"Начальная позиция (фактическая): {pose1} ")
    print(f"Вторая позиция (фактическая): {pose2} ")
    
    # Симуляция сканирования
    angle_step = 1.0    # Шаг сканирования, в градусах
    ranges1, angles = simulation_laser.simulateLaserScan(pos1, th1, polygon, angle_step_deg=angle_step)
    ranges2, _ = simulation_laser.simulateLaserScan(pos2, th2, polygon, angle_step_deg=angle_step)
    
    # Построение карты для первого скана
    pts1_local, mask1 = simulation_laser.scanToPointsLocal(ranges1, angles)
    pts1_world = utils_poses_and_points.transformPoints(pts1_local, pose1)
    
    grid, origin, res = occupancy_grid.buiildOccupancyGrid(pts1_world, resolution=0.05, padding=0.5)

    # Сканирование карты brute-force
    pose_delta_bf, best_pose_bf, score_bf = brute_force.doScanMatchingMapBruteForce(grid, origin, res, pose1, ranges2, angles, search_radius=max(w, h), coarse_steps=(30, 30, 52), refine_iters=2)
    # pose_delta_bf, best_pose_bf, score_bf = brute_force.doScanMatchingMapBruteForce(grid, origin, res, pose1, ranges2, angles, search_radius=2.0, coarse_steps=(21, 21, 36), refine_iters=2)
    
    found_pose2_bf = best_pose_bf
    trans_error_bf = np.linalg.norm(found_pose2_bf[:2] - pose2[:2])
    rot_error_bf = abs(utils_poses_and_points.wrapToPi(found_pose2_bf[2] - pose2[2]))
    
    print("\n---- Результат scan-to-map brute-force ---")
    print("Найденная абсолютная позиция сканера2: ", *found_pose2_bf)
    print("Поза-дельта (pose_inv(pose1) * found_pose2): ", pose_delta_bf)
    print("score (доля точек попавших в занятость): ", score_bf)
    print("Ошибка трансляции (m): ", trans_error_bf)
    print("Ошибка поворота (rad): ", rot_error_bf)
    
    # scan-to-scan ICP (best_pose_bf как init)
    found_pose_icp, pose_delta_icp = icp.doICP(pts1_world, ranges2, angles, found_pose2_bf)
    trans_error_icp = np.linalg.norm(found_pose_icp[:2] - pose2[:2])
    rot_error_icp = abs(utils_poses_and_points.wrapToPi(found_pose_icp[2] - pose2[2]))
    
    print("\n---- Результат scan-to-scan ICP ---")
    print("Найденная абсолютная позиция сканера2: ", *found_pose_icp)
    print("Ошибка трансляции (m): ", trans_error_icp)
    print("Ошибка поворота (rad): ", rot_error_icp)
    
    
    print(f"Точность определения положения (%): {trans_error_bf / max(w, h) * 100:.2f}% для bf, {trans_error_icp / max(w, h) * 100:.2f}% для ICP")
    
    # Отрисовка: комната, оба скана и карты
    plt.figure(figsize=(5,3))
    plt.subplot(1, 2, 1)                # (строки, стоблцы, индекс)
    plt.title("Room and scans (world)")
    poly = np.vstack((polygon, polygon[0]))
    plt.plot(poly[:,0], poly[:,1], "-k")                                    # построить полигон
    plt.scatter([pose1[0]], pose1[1], c="blue", label="scanner 1")          # Начальное положение в полигоне
    plt.scatter([pose2[0]], pose2[1], c="green", label="scanner 2 (true)")  # Конечное положение в полигоне (фактическое)
    plt.scatter([found_pose2_bf[0]], [found_pose2_bf[1]], c="red", label="found pose (bf)")         # Конечное положение в полигоне (bf)
    plt.scatter([found_pose_icp[0]], [found_pose_icp[1]], c="magenta", label="found pose (icp)")    # Конечное положение в полигоне (ICP)
    pts2_local, _ = simulation_laser.scanToPointsLocal(ranges2, angles)
    pts2_world_true = utils_poses_and_points.transformPoints(pts2_local, pose2)
    pts2_word_found = utils_poses_and_points.transformPoints(pts2_local, found_pose_icp)
    
    # Отрисовка точек сканирования
    plt.scatter(pts1_world[:,0], pts1_world[:,1], s=16, label="scan1 points")                       
    plt.scatter(pts2_world_true[:,0], pts2_world_true[:,1], s=16, label="scan2 true", alpha=0.6)
    # plt.scatter(pts2_word_found[:,0], pts2_word_found[:,1], s=6, label="scan2 icp", alpha=0.6)
    plt.axis("equal")
    plt.legend()
    
    plt.subplot(1, 2, 2)
    plt.title("Occupancy grid (from scan1)")
    plt.imshow(grid, origin="lower", extent=(origin[0], origin[0]+grid.shape[1]*res, origin[1], origin[1]+grid.shape[0]*res))
    plt.scatter(pts2_world_true[:,1], pts2_world_true[:,0], c="magenta", s=12, label="scan2 true", alpha=0.6)
    # plt.scatter(pts2_word_found[:,1], pts2_word_found[:,0], c="blue",s=12, label="scan2 icp", alpha=0.6)
    plt.axis("equal")
    plt.legend()
    plt.show()
    





if __name__ == "__main__":
    main()