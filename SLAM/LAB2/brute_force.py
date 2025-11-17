#   Сканирование для сопоставления методом brute-force

import simulation_laser
import utils_poses_and_points

import numpy as np
from math import pi

from typing import List, Tuple
Point = Tuple[float, float]
Polygon = List[Point]
Edge = Tuple[Point, Point]


def scoreScanOnMap(grid: List, origin: Point, resolution: float, points_world: List[Point]) -> float:
    """
    Оценивает, насколько хорошо скан (набор точек) совпадает с картой (сеткой занятости)

    Args:
        grid (List): Двумерный массив, представляющий сетку занятости (occupancy grid)
            1 — занятая ячейка
            0 — свободная ячейка
        origin (Point): Начало координат сетки (левый нижний угол)
        resolution (float): Размер одной ячейки сетки
        points_world (List[Point]): Список точек скана в глобальных координатах

    Returns:
        float:  Оценка (score [0 ; 1]) совпадения скана с картой = среднее значение занятости ячеек, на которые попали точки скана
    """
    nx, ny = grid.shape     # количество ячеек по осям x и y

    
    # ix = ((points_world[:,0] - origin[0]) / resolution).astype(int)    # округление до ближайшего целого числа (индекс ячейки)        
    # iy = ((points_world[:,1] - origin[1]) / resolution).astype(int)
    # смещение точки относительно начала сетки, делённое на размер ячейки
    ix = np.floor((points_world[:,0] - origin[0]) / resolution).astype(int)
    iy = np.floor((points_world[:,1] - origin[1]) / resolution).astype(int)
    # маска проверяет, что индексы ix и iy находятся в пределах сетки
    valid = (ix >= 0) & (ix < nx) & (iy >= 0) & (iy < ny)  
    ix = ix[valid]
    iy = iy[valid]
    
    if len(ix) == 0:    return 0        # если ни одна точка не попала в границы сетки, функция возвращает 0 (нет совпадений
    
    # Среднее значение занятости ячеек, на которые попали точки скана
    # Чем ближе это значение к 1, тем лучше скан совпадает с картой
    return grid[ix, iy].sum() / float(len(points_world)) 





def doScanMatchingMapBruteForce(grid: List, origin: Point, resolution: float, pose1: List[float], ranges2: List, angles2: List,
                                search_radius=2.0,
                                coarse_steps=(21, 21, 36), 
                                refine_iters=2) -> Tuple:
    """
    Реализация метода грубого перебора (brute-force) для сопоставления скана с картой (scan matching)
    Поиск позы робота, при которой скан лучше всего совпадает с имеющейся картой (occupancy grid)

    Args:
        grid (List): Сетка занятости (occupancy grid) — двумерный массив, 1 - занятая ячейка, 0 - свободная
        origin (Point): Начало координат сетки (левый нижний угол)
        resolution (float):  Размер одной ячейки сетки
        pose1 (List[float, float, float]): Начальная поза робота (предыдущая оценка его положения)
        ranges2 (List): Данные скана - массив расстояний
        angles2 (List): Данные скана - массив углов
        search_radius (float): Радиус области поиска вокруг начальной позы. Defaults to 2.0.
        coarse_steps (tuple): Количество шагов по осям x, y и углу theta на каждом уровне поиска
        refine_iters (int): Количество итераций уточнения. Defaults to 2.

    Returns:
        Tuple[float, float, float]:  Кортеж (pose_delta, best_pose, best_score), где:
            pose_delta — разница между начальной и лучшей найденной позой
            best_pose — лучшая найденная поза
            best_score — оценка совпадения для лучшей позы
    """

    pts_local2, mask = simulation_laser.scanToPointsLocal(ranges2, angles2) # преобразование скана 2 в локальные точки
    
    # Проверка на пустые данные скана
    if len(pts_local2) == 0:
        return np.zeros(3), pose1.copy(), 0.0
    
    # начальный центр: позиция 1
    center = pose1.copy()       # текущая "центральная" поза для поиска (изначально равна pose1)
    best_pose = None            # лучшая найденная поза (изначально None)
    best_score = -1             # лучшая оценка совпадения (изначально -1)
    
    # многоуровневый перебор: сначала грубый, затем уточнения
    search_radius_level = search_radius     # Текущий радиус поиска (уменьшается на каждой итерации)
    
    # print("center ", center)
    for it in range(refine_iters):          # Количество итераций уточнения (по умолчанию 2)
        nx, ny, ntheta = coarse_steps
        # print("search_radius_level ", search_radius_level)
        # print("nx, ny, ntheta ", nx, ny, ntheta)
        # массивы смещений по осям x и y в пределах [-search_radius_level, search_radius_level] с количеством шагов nx и ny
        dxs = np.linspace(start=-search_radius_level, stop=search_radius_level, num=nx)
        dys = np.linspace(start=-search_radius_level, stop=search_radius_level, num=ny)
        # (it == 0) — полный диапазон [-pi, pi] с ntheta шагами
        thetas = np.linspace(start=-pi, stop=pi, num=ntheta, endpoint=False) if it==0 else np.linspace(start=-0.5, stop=0.5, num=ntheta)  #  массив угловых смещений
        # print("dxs ", dxs)
        # print("dys ", dys)
        # print("thetas ", thetas, '\n')
        best_local_pose = None  # лучшая поза на текущем уровне поиска
        best_local_score = -1   # лучшая оценка совпадения на текущем уровне поиска
        
        # Перебор всех возможных поз
        for dx in dxs:
            for dy in dys:
                # быстрый предварительный перевод
                trans = np.array([center[0] + dx, center[1] + dy])  # новая позиция (x, y) для текущего смещения
                for dth in thetas:
                    cand_pose = np.array([trans[0], trans[1], utils_poses_and_points.wrapToPi(center[2] + dth)])    # кандидат на лучшую позу — текущая позиция и угол
                    pts_world = utils_poses_and_points.transformPoints(pts_local2, cand_pose)   # преобразование точек скана из локальной системы координат в глобальную, используя текущую кандидатскую позу cand_pose

                    # Проверка на пустые точки после преобразования
                    if len(pts_world) == 0:     continue
                    
                    # обновление лучшей позы
                    sc = scoreScanOnMap(grid, origin, resolution, pts_world)    # вычисляет оценку совпадения точек скана с картой (occupancy grid)
                    if sc > best_local_score:
                        best_local_score = sc
                        best_local_pose = cand_pose
        
        # Если не нашли лучшую позу, используем текущую
        if best_local_score > best_score:
            # обновляются на лучшие значения
            best_score = best_local_score
            best_pose = best_local_pose
        
        
        # уточненный центр — best_local_pose
        center = best_local_pose        # обновляется на лучшую найденную позу
    
        
        # радиуса меньшается в 4 раза для следующей итерации (уточнение)
        search_radius_level = search_radius_level / 4.0
        
        coarse_steps = (11, 11, 41) # На следующей итерации используется меньше шагов для более точного поиска
        
    #  Вычисление разницы между начальной и лучшей позой
    # poseInv(pose1): Обратная поза для начальной позы pose1
    # poseMul: Композиция обратной позы и лучшей найденной позы, чтобы получить разницу между ними
    pose_delta = utils_poses_and_points.poseMul(utils_poses_and_points.poseInv(pose1), best_pose)   
    return pose_delta, best_pose, best_score