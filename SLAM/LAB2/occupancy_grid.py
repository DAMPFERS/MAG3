#   Построение occupancy-grid карты

import numpy as np
from bresenham import bresenhamLine

from typing import List, Tuple
Point = Tuple[float, float]
Polygon = List[Point]
Edge = Tuple[Point, Point]


def buiildOccupancyGrid(points_world: List[Point], resolution=0.05, padding=0.5) -> Tuple:
    """
    Построение сетки занятости (occupancy grid) на основе массива точек в глобальных координатах

    Args:
        points_world (List[Point]): Список точек в глобальной системе координат
        resolution (float): Размер ячейки сетки. Defaults to 0.05.
        padding (float):  Отступ от крайних точек для расширения границ сетки. Defaults to 0.5.

    Returns:
        Tuple: 
            grid — двумерный массив, представляющий сетку занятости
            min_xy — минимальные координаты (левый нижний угол сетки)
            resolution — размер ячейки сетки
    """
    
    min_xy = points_world.min(axis=0) - padding # минимальные координаты по осям x и y среди всех точек
    max_xy = points_world.max(axis=0) + padding # максимальные координаты по осям x и y среди всех точек
    size = max_xy - min_xy                      # размер области, которую покрывает сетка, в метрах
    
    # количество ячеек по осям x и y, округлённое вверх до целого числа
    nx = int(np.ceil(size[0] / resolution))     
    ny = int(np.ceil(size[1] / resolution))
    
    grid = np.zeros((nx, ny), dtype=np.uint8)   # Двумерный массив нулей размером (nx, ny), каждая ячейка соответствует одной ячейке сетки, тип данных целое число от 0 до 255
    
    # для каждой точки вычисляется её положение в сетке
    # inds_x = ((points_world[:,0] - min_xy[0]) / resolution).astype(int)     # преобразование координаты x точки в индекс ячейки по оси x
    # inds_y = ((points_world[:,1] - min_xy[1]) / resolution).astype(int)     # преобразование координаты y точки в индекс ячейки по оси y
    inds_x = np.floor((points_world[:,0] - min_xy[0]) / resolution).astype(int) # преобразование координаты x точки в индекс ячейки по оси x
    inds_y = np.floor((points_world[:,1] - min_xy[1]) / resolution).astype(int)
    
    
    # фильтрация корректных индексов
    valid = (inds_x >= 0) & (inds_x < nx) & (inds_y >= 0) & (inds_y < ny)   # логическая маска, которая проверяет, что индексы находятся в пределах сетки
    # фильтруются только те индексы, которые попадают в границы сетки
    inds_x = inds_x[valid]
    inds_y = inds_y[valid]
    grid[inds_x, inds_y] = 1                                                # в ячейках сетки, соответствующих координатам точек, устанавливается значение 1 (занятая ячейка)
    
    return grid, min_xy, resolution


def updateOccupancyGrid(grid: np.ndarray, origin: Point, resolution: float, 
                       scanner_pose: List[float], points_world: List[Point],
                       free_weight=0.3, occupied_weight=0.7) -> np.ndarray:
    """
    Обновляет occupancy grid на основе нового скана
    
    Args:
        grid: текущая сетка занятости
        origin: начало координат сетки
        resolution: размер ячейки
        scanner_pose: поза сканера [x, y, theta]
        points_world: точки скана в глобальных координатах
        free_weight: вес для свободных ячеек
        occupied_weight: вес для занятых ячеек
    
    Returns:
        Обновленная сетка занятости
    """
    grid = grid.astype(np.float32)  # перевод в float для весов
    
    # преобразование позы сканера в координаты сетки
    scanner_x, scanner_y = scanner_pose[:2]
    scanner_ix = int((scanner_x - origin[0]) / resolution)
    scanner_iy = int((scanner_y - origin[1]) / resolution)
    
    # обновление занятых ячеек (точки попадания луча)
    for point in points_world:
        px, py = point
        ix = int((px - origin[0]) / resolution)
        iy = int((py - origin[1]) / resolution)
        
        if 0 <= ix < grid.shape[0] and 0 <= iy < grid.shape[1]:
            grid[ix, iy] += occupied_weight
            
    # обновление свободных ячеек (лучи от сканера до точек)
    for point in points_world:
        px, py = point
        ix = int((px - origin[0]) / resolution)
        iy = int((py - origin[1]) / resolution)
        
        # получение всех ячеек вдоль луча
        line_points = bresenhamLine((scanner_ix, scanner_iy), (ix, iy))
        
        for cell_ix, cell_iy in line_points[:-1]:  # Исключаем конечную точку (она занятая)
            if 0 <= cell_ix < grid.shape[0] and 0 <= cell_iy < grid.shape[1]:
                grid[cell_ix, cell_iy] -= free_weight
    
    # нормализация значения в диапазон [0, 1]
    grid = np.clip(grid, 0, 1)
    
    return grid