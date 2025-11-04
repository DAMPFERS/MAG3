"""
АПИМ-24, Разуваев В.В.
SLAM
Лабораторная работа №1: генерация комнаты (прямоугольник с скошенным углом), симуляция двух 2D лазерных сканов
и реализация скан-матчинга:

"""

import numpy as np
import matplotlib.pyplot as plt
from math import cos, sin, pi, sqrt
import time



import utils_poses_and_points
import geometry_room
import simulation_laser
import occupancy_grid
import brute_force
import icp
import anim_slam


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



def main0():
    np.random.seed(0)
    # w, h, cut = 6.2, 4.6, 1.0
    w, h, cut = 4.5, 6.2, 2.4
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
    th1 = np.random.uniform(low=-pi, high=pi)    # Начальный угол поворота
    # pos2 = sampleInPoly(polygon)    # Вторая позиция
    pos2 = sampleInPolyNearPoint(polygon, center=pos1, radius=2.0)    # Вторая позиция
    th2 = np.random.uniform(low=-pi, high=pi)    # Конечный угол поворота
    
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
    
    grid, origin, res = occupancy_grid.buiildOccupancyGrid(pts1_world, resolution=0.2, padding=0.5)

    # Сканирование карты brute-force
    # pose_delta_bf, best_pose_bf, score_bf = brute_force.doScanMatchingMapBruteForce(grid, origin, res, pose1, ranges2, angles, search_radius=max(w, h), coarse_steps=(30, 30, 52), refine_iters=2)
    pose_delta_bf, best_pose_bf, score_bf = brute_force.doScanMatchingMapBruteForce(grid, origin, res, pose1, ranges2, angles, search_radius=2.5, coarse_steps=(21, 21, 36), refine_iters=2)
    
    found_pose2_bf = best_pose_bf
    trans_error_bf = np.linalg.norm(found_pose2_bf[:2] - pose2[:2])
    rot_error_bf = abs(utils_poses_and_points.wrapToPi(found_pose2_bf[2] - pose2[2]))

    print("\n---- Результат scan-to-map brute-force ---")
    print("Найденная абсолютная позиция сканера2: ", *found_pose2_bf)
    print("Поза-дельта (pose_inv(pose1) * found_pose2): ", pose_delta_bf)
    print("score: ", score_bf)
    print("Ошибка трансляции (m): ", trans_error_bf)
    print("Ошибка поворота (rad): ", rot_error_bf)
    
   
    
    
    print(f"Точность определения положения (%): {trans_error_bf / sqrt(w*w + h*h) * 100:.2f}% для bf")
    
    # Отрисовка: комната, оба скана и карты
    plt.figure(figsize=(5,3))
    plt.subplot(1, 2, 1)                # (строки, стоблцы, индекс)
    plt.title("Room and scans (world)")
    poly = np.vstack((polygon, polygon[0]))
    plt.plot(poly[:,0], poly[:,1], "-k")                                    # построить полигон
    plt.scatter([pose1[0]], pose1[1], c="blue", label="scanner 1")          # Начальное положение в полигоне
    plt.scatter([pose2[0]], pose2[1], c="green", label="scanner 2 (true)")  # Конечное положение в полигоне (фактическое)
    plt.scatter([found_pose2_bf[0]], [found_pose2_bf[1]], c="red", label="found pose (bf)")         # Конечное положение в полигоне (bf)
    pts2_local, _ = simulation_laser.scanToPointsLocal(ranges2, angles)
    pts2_world_true = utils_poses_and_points.transformPoints(pts2_local, pose2)
    
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

    plt.axis("equal")
    plt.legend()
    
    
    plt.show()
    

def main():
    np.random.seed(1)
    w, h, cut = 4.5, 6.2, 2.4
    polygon = geometry_room.makeSkewedRectangle(w, h, cut)
    
    # вычисление реальную площадь комнаты
    def calculatePolygonArea(poly: Polygon) -> float:
        """Вычисляет площадь полигона по формуле шнурования"""
        x = poly[:, 0]
        y = poly[:, 1]
        return 0.5 * np.abs(np.dot(x, np.roll(y, 1)) - np.dot(y, np.roll(x, 1)))
    
    real_area = calculatePolygonArea(poly=polygon)
    print(f"Реальная площадь комнаты: {real_area:.2f} кв.м.")
    
    # генерация последовательности поз (10-20 сканов)
    flag_ICP = False
    num_scans = 20
    resolution = 0.15
    
    poses = []
    
    # Начальная поза
    current_pose = np.array([w/2, h/2, 0.0])  # примерно в центре
    
    for i in range(num_scans):
        # Генерируем следующую позу (просто случайное смещение)
        if i == 0:
            poses.append(current_pose)
        else:
            dx = np.random.uniform(-0.5, 0.5)
            dy = np.random.uniform(-0.5, 0.5)
            dtheta = np.random.uniform(-0.3, 0.3)
            
            new_pose = current_pose.copy()
            new_pose[0] += dx
            new_pose[1] += dy  
            new_pose[2] = utils_poses_and_points.wrapToPi(new_pose[2] + dtheta)
            
            # Проверяем, что новая поза внутри полигона
            if pointInPoly(new_pose[:2], polygon):
                poses.append(new_pose)
                current_pose = new_pose
    
    print(f"Сгенерировано {len(poses)} поз для SLAM")
    
    # Инициализация карты
    grid = None
    origin = None
    
    
    # SLAM цикл
    estimated_poses = []
    
    for i, true_pose in enumerate(poses):
        print(f"\n--- Обработка скана {i+1}/{len(poses)} ---")
        
        # Симуляция сканирования
        ranges, angles = simulation_laser.simulateLaserScan(
            true_pose[:2], true_pose[2], polygon, angle_step_deg=1.0
        )
        
        # Преобразование скана в точки
        pts_local, mask = simulation_laser.scanToPointsLocal(ranges, angles)
        
        if len(pts_local) == 0:
            print(f"Скан {i+1}: нет валидных точек, пропускаем")
            continue
        
        if i == 0:
            # инициализация карты по первому скану
            pts_world = utils_poses_and_points.transformPoints(pts_local, true_pose)
            grid, origin, resolution = occupancy_grid.buiildOccupancyGrid(
                pts_world, resolution=resolution, padding=1.0
            )
            estimated_poses.append(true_pose)
            print(f"Инициализирована карта размером {grid.shape}")
            
        else:
            # Скан-матчинг для определения позы относительно текущей карты
            prev_pose = estimated_poses[-1]
            
            pose_delta, best_pose, score = brute_force.doScanMatchingMapBruteForce(
                grid, origin, resolution, prev_pose, ranges, angles,
                search_radius=1.2, coarse_steps=(15, 15, 24), refine_iters=2
            )
            
            # ICP
            if flag_ICP:    best_pose, pose_delta_icp = icp.doICP(pts_world, ranges, angles, best_pose)
            
            estimated_poses.append(best_pose)
            print(f"Найдена поза: {best_pose}, score: {score:.3f}")
            
            # Обновление карты
            pts_world = utils_poses_and_points.transformPoints(pts_local, best_pose)
            grid = occupancy_grid.updateOccupancyGrid(grid, origin, resolution, best_pose, pts_world)
        
    # вычисление площади построенной карты
    occupied_cells = np.sum(grid < 0.5)  # Ячейки с вероятностью занятости < 0.5
    built_area = occupied_cells * (resolution ** 2) / 2
    
    print(f"\n=== РЕЗУЛЬТАТЫ SLAM ===")
    print(f"Реальная площадь: {real_area:.2f} м²")
    print(f"Построенная площадь: {built_area:.2f} м²")
    print(f"Относительная ошибка: {abs(real_area - built_area)/real_area*100:.1f}%")   
    
    # визуализация результатов
    plt.figure(figsize=(12, 5))
    
    # исходная комната и траектория
    plt.subplot(1, 2, 1)
    plt.title("Исходная комната и траектория")
    poly_plot = np.vstack((polygon, polygon[0]))
    plt.plot(poly_plot[:, 0], poly_plot[:, 1], 'k-', linewidth=2, label='Комната')
    
    # траектория
    poses_array = np.array(poses)
    est_poses_array = np.array(estimated_poses)
    plt.plot(poses_array[:, 0], poses_array[:, 1], 'go-', markersize=4, label='Истинная траектория')
    plt.plot(est_poses_array[:, 0], est_poses_array[:, 1], 'ro-', markersize=4, label='Оцененная траектория')
    plt.legend()
    plt.axis('equal')
    plt.grid(True)
    
    #  карта
    plt.subplot(1, 2, 2)
    plt.title("Построенная карта (Occupancy Grid)")
    plt.imshow(grid.T, origin='lower', 
              extent=[origin[0], origin[0] + grid.shape[0] * resolution,
                      origin[1], origin[1] + grid.shape[1] * resolution],
              cmap='binary')
    plt.colorbar(label='Вероятность занятости')
    plt.plot(est_poses_array[:, 0], est_poses_array[:, 1], 'ro-', markersize=3, label='Траектория')
    plt.legend()
    plt.axis('equal')
    
    plt.tight_layout()
    plt.show()
    

def main2():
    np.random.seed(0)
    w, h, cut = 4.5, 6.2, 2.4
    polygon = geometry_room.makeSkewedRectangle(w, h, cut)
    
    # Настройка интерактивного графика
    fig, ax1, ax2 = anim_slam.setupRealTimePlot()
    
    # Генерация последовательности поз (10-20 сканов)
    num_scans = 20
    poses = []
    
    # Начальная поза
    current_pose = np.array([w/2, h/2, 0.0])
    
    for i in range(num_scans):
        if i == 0:
            poses.append(current_pose)
        else:
            dx = np.random.uniform(-0.5, 0.5)
            dy = np.random.uniform(-0.5, 0.5)
            dtheta = np.random.uniform(-0.3, 0.3)
            
            new_pose = current_pose.copy()
            new_pose[0] += dx
            new_pose[1] += dy  
            new_pose[2] = utils_poses_and_points.wrapToPi(new_pose[2] + dtheta)
            
            if pointInPoly(new_pose[:2], polygon):
                poses.append(new_pose)
                current_pose = new_pose
    
    print(f"Сгенерировано {len(poses)} поз для SLAM")
    
    # Инициализация SLAM
    grid = None
    origin = None
    resolution = 0.15
    estimated_poses = []
    
    # Данные для визуализации
    scan_data = {
        'polygon': polygon,
        'poses': poses,
        'estimated_poses': estimated_poses,
        'current_scan_points': None,
        'grid': grid,
        'origin': origin,
        'resolution': resolution,
        'current_scan_num': 0,
        'total_scans': len(poses)
    }
    
    # SLAM цикл с визуализацией
    for i, true_pose in enumerate(poses):
        print(f"\n--- Обработка скана {i+1}/{len(poses)} ---")
        
        # Симуляция сканирования
        ranges, angles = simulation_laser.simulateLaserScan(
            true_pose[:2], true_pose[2], polygon, angle_step_deg=2.0
        )
        
        # Преобразование скана в точки
        pts_local, mask = simulation_laser.scanToPointsLocal(ranges, angles)
        
        if len(pts_local) == 0:
            print(f"Скан {i+1}: нет валидных точек, пропускаем")
            continue
        
        if i == 0:
            # Инициализация карты по первому скану
            pts_world = utils_poses_and_points.transformPoints(pts_local, true_pose)
            grid, origin, resolution = occupancy_grid.buiildOccupancyGrid(
                pts_world, resolution=resolution, padding=1.0
            )
            estimated_poses.append(true_pose)
            print(f"Инициализирована карта размером {grid.shape}")
        else:
            # Скан-матчинг для определения позы
            prev_pose = estimated_poses[-1]
            
            pose_delta, best_pose, score = brute_force.doScanMatchingMapBruteForce(
                grid, origin, resolution, prev_pose, ranges, angles,
                search_radius=1.0, coarse_steps=(15, 15, 24), refine_iters=2
            )
            
            estimated_poses.append(best_pose)
            print(f"Найдена поза: {best_pose}, score: {score:.3f}")
            
            # Обновление карты
            pts_world = utils_poses_and_points.transformPoints(pts_local, best_pose)
            grid = occupancy_grid.updateOccupancyGrid(grid, origin, resolution, best_pose, pts_world)
        
        # Обновление данных для визуализации
        scan_data.update({
            'estimated_poses': estimated_poses,
            'current_scan_points': pts_world if i > 0 else utils_poses_and_points.transformPoints(pts_local, true_pose),
            'grid': grid,
            'origin': origin,
            'current_scan_num': i + 1
        })
        
        # Обновление графика
        anim_slam.updateRealTimePlot(ax1, ax2, scan_data)
        
        # Небольшая пауза для наглядности
        time.sleep(0.5)
    
    # Финальная визуализация и метрики
    plt.figure(figsize=(10, 8))
    
    # Финальный график с метриками
    real_area, built_area, area_error, avg_position_error = anim_slam.сalculateAndDisplayMetrics(
        polygon, grid, origin, resolution, poses, estimated_poses
    )
    
    # Финальная визуализация
    plt.figure(figsize=(12, 5))
    
    plt.subplot(1, 2, 1)
    plt.title("Финальная траектория")
    poly_plot = np.vstack((polygon, polygon[0]))
    plt.plot(poly_plot[:, 0], poly_plot[:, 1], 'k-', linewidth=2, label='Комната')
    
    poses_array = np.array(poses)
    est_poses_array = np.array(estimated_poses)
    plt.plot(poses_array[:, 0], poses_array[:, 1], 'go-', markersize=4, label='Истинная траектория')
    plt.plot(est_poses_array[:, 0], est_poses_array[:, 1], 'ro-', markersize=4, label='Оцененная траектория')
    plt.legend()
    plt.axis('equal')
    plt.grid(True)
    
    plt.subplot(1, 2, 2)
    plt.title(f"Финальная карта (ошибка: {area_error:.1f}%)")
    plt.imshow(grid.T, origin='lower', 
              extent=[origin[0], origin[0] + grid.shape[0] * resolution,
                      origin[1], origin[1] + grid.shape[1] * resolution],
              cmap='RdYlBu_r', alpha=0.8)
    plt.colorbar(label='Вероятность занятости')
    plt.plot(est_poses_array[:, 0], est_poses_array[:, 1], 'ro-', markersize=3)
    plt.axis('equal')
    
    plt.tight_layout()
    plt.show()




if __name__ == "__main__":
    main2()