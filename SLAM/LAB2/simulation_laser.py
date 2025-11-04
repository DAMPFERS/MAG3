#   Симуляция лазерного скана

import numpy as np
from math import cos, sin
import geometry_room

from typing import List, Tuple
Point = Tuple[float, float]
Polygon = List[Point]
Edge = Tuple[Point, Point]




def simulateLaserScan(scanner_pos: Point, scanner_theta: float, polygon : Polygon, angle_step_deg=1.0, max_range=20.0) -> List:
    """
    Симулирует 2D лазерный скан. Возвращает ranges (N,) и angles (N,) в локальной системе сканера.

    Args:
        scanner_pos (Point): позиция сканера (пара координат)
        scanner_theta (float): Угол поворота сканера
        polygon (Polygon): Список вершин полигона
        angle_step_deg (float): Шаг угла сканирования. Defaults to 1.0.
        max_range (float): Максимально возможное расстояние от сканера до ребер. Defaults to 20.0.

    Returns:
        list: Список расстояний и соответствующих углов
    """
    
    edges = geometry_room.polygonEdges(polygon) # Получаем список ребер полигона
    step = np.deg2rad(angle_step_deg)           # Перевод градусов в радианы
    angles = np.arange(0, 2*np.pi, step)        # Получение списка углов сканирования с заданным шагом (от 0 до 2пи)
    ranges = np.full_like(angles, max_range, dtype=float)   # Создается список для хранения расстояний лазерного сканирования (инициируется максимальным значением)
    
    P = np.array(scanner_pos)
    for i, a in enumerate(angles):          # Перебор углов
        ang_world = scanner_theta + a       # Счетчик угла поворота сканера
        r = np.array([cos(ang_world), sin(ang_world)]) # Массив
        min_t = max_range
        for A, B in edges:                  # Перебор ребер
            t = geometry_room.raySegmentIntersection(P, r, A, B)
            if t is not None and t < min_t:     min_t = t       # Если найденное расстояние не выходит за пределы, заносится в список
            ranges[i] = min_t
    # Возвращенные углы находятся в кадре сканера (0..2pi)
    return ranges, angles    
    

def scanToPointsLocal(ranges: List, angles: List) -> List[Point]:
    """
    Преобразование скана в точки

    Args:
        ranges (List): Массив расстояний
        angles (List): Массив Углов

    Returns:
        List: Массив точек сканированного полигона (локальные координаты) и маска
    """
    
    xs = ranges * np.cos(angles)
    ys = ranges * np.sin(angles)
    pts = np.stack((xs, ys), axis=1) # Объединение массивов координат в массив точек (объединяются по столбцам)
    mask = ranges < np.max(ranges) # Отфильтровать точки на максимальном расстоянии (без попаданий)
    return pts[mask], mask


    
if __name__ == "__main__":
    pass