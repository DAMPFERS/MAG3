#   Построение occupancy-grid карты

import numpy as np

from typing import List, Tuple
Point = Tuple[float, float]
Polygon = List[Point]
Edge = Tuple[Point, Point]


def buiildOccupancyGrid(points_world: List[Point], resolution=0.05, padding=0.5) -> Tuple:
    """
    Построение сетки занятости (occupancy grid) на основе массива точек в глобальных координатах

    Args:
        points_world (List[Point]): Список точек в глобальной системе координат
        resolution (float, optional): Размер ячейки сетки. Defaults to 0.05.
        padding (float, optional):  Отступ от крайних точек для расширения границ сетки. Defaults to 0.5.

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
    inds_x = ((points_world[:,0] - min_xy[0]) / resolution).astype(int)     # преобразование координаты x точки в индекс ячейки по оси x
    inds_y = ((points_world[:,1] - min_xy[1]) / resolution).astype(int)     # преобразование координаты y точки в индекс ячейки по оси y
    
    # фильтрация корректных индексов
    valid = (inds_x >= 0) & (inds_x < nx) & (inds_y >= 0) & (inds_y < ny)   # логическая маска, которая проверяет, что индексы находятся в пределах сетки
    # фильтруются только те индексы, которые попадают в границы сетки
    inds_x = inds_x[valid]
    inds_y = inds_y[valid]
    grid[inds_x, inds_y] = 1                                                # в ячейках сетки, соответствующих координатам точек, устанавливается значение 1 (занятая ячейка)
    
    return grid, min_xy, resolution