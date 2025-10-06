import numpy as np
import matplotlib.pyplot as plt
from math import cos, sin, atan2, pi
from typing import List, Tuple


import utils_poses_and_points
import geometry_room
import simulation_laser
import occupancy_grid
import brute_force


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
    
    
    
    pos1 = sampleInPoly(polygon)    # Начальная позиция
    th1 = np.random.uniform(-pi, pi)    # Начальный угол поворота
    pos2 = sampleInPoly(polygon)    # Вторая позиция
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
    





if __name__ == "__main__":
    main()