# Scan-to-Scan ICP (point-to-point)

import numpy as np
from math import atan2

from typing import List, Tuple
Point = Tuple[float, float]
Polygon = List[Point]
Edge = Tuple[Point, Point]


import simulation_laser
import utils_poses_and_points


def nearestNeighbors(src: List[Point], dst: List[Point]) -> Tuple:
    """
    Для каждой точки из src находит индекс ближайшей точки в dst и расстояние до неё

    Args:
        src (List[Point]): _description_
        dst (List[Point]): _description_

    Returns:
        Tuple:   inds - массив индексов ближайших точек из dst для каждой точки из src
                dists - массив расстояний до ближайших точек
    """
    
    # инициализация массивов нулями
    inds = np.zeros(len(src), dtype=int)
    dists = np.zeros(len(src), dtype=float)
    
    for i,p in enumerate(src): # перебор точек из src 
        dif = dst - p                   # массив разностей координат между каждой точкой из dst и текущей точкой p из src ((x2 - x1, y2 - y1), ...)
        dd = np.sum(dif*dif, axis=1)    # вычисление квадратов расстояний: массив квадратов расстояний от точки p до каждой точки из dst
        j = np.argmin(dd)               # индекс минимального элемента в массиве dd (индекс ближайшей точки из dst к точке p)
        
        inds[i] = j                     # cохранение индекса ближайшей точки
        dists[i] = np.sqrt(dd[j])       # cохранение расстояния до ближайшей точки
    
    return inds, dists




def bestFitTransform(A: List[Point], B: List[Point]) -> Tuple:
    """
    Вычисление оптимального преобразования (поворота и сдвига), минимизирующего расстояние между двумя наборами точек A и B

    Args:
        A (List[Point]): Список точек из первой группы
        B (List[Point]): Список точек из второй группы

    Returns:
        Tuple: Кортеж где:
            R: Матрица поворота (2*2)
            t: Вектор сдвига
            angle: Угол поворота (в радианах)
    """
    
    assert A.shape == B.shape       # проверка, что A и B имеют одинаковое количество точек
    N = A.shape[0]                  # колличество точекв наборе
    
    # центроиды вычисляются как среднее значение координат по всем точкам
    centroid_A = A.mean(axis=0)
    centroid_B = B.mean(axis=0)
    
    # центрирование точек (точки наборов сдвинуты чтобы их центроиды находились в начале координат)
    AA = A - centroid_A
    BB = B - centroid_B
    
    # для нахождения оптимального поворота
    H = AA.T.dot(BB)            #  матрица ковариации между центрированными точками AA и BB
    
    # сингулярное разложение (SVD) матрицы H
    U, S, Vt = np.linalg.svd(H) #  левая ортогональная матрица, диагональная матрица сингулярных чисел, транспонированная правая ортогональная матрица
    
    # задача ортогонального Прокрастова анализа
    R = Vt.T.dot(U.T)       # матрица поворота,  минимизирует расстояние между центрированными точками AA и BB
    
    if np.linalg.det(R) < 0:        # если определитель отрицательный, это значит, что матрица R содержит отражение, (плохо)
         # смена знака последней строки матрицы Vt и пересчет R
        Vt[-1,:] *= -1             
        R = Vt.T.dot(U.T)
        
    t = centroid_B - R.dot(centroid_A)  # вектор сдвига,  перемещает центроид набора A в центроид набора B после поворота
    
    angle = atan2(R[1,0], R[0,0])       # угол поворота, соответствующий матрице R
    return R, t, angle



def doICP(scan1_world_pts: List[Point], ranges2: List, angles2: List, init_pose2: List[float], max_iters=20, tol=1e-4) -> Tuple:
    """
    Реализация алгоритма Iterative Closest Point (ICP)
    Сопоставление двух облаков точек и нахождения такого преобразования (сдвига и поворота), при котором эти облака лучше всего совпадают

    Args:
        scan1_world_pts (List[Point]): Список точек первого скана в глобальных координатах
        ranges2 (List): Данные второго скана - массив расстояний
        angles2 (List): Данные второго скана - массив углов
        init_pose2 (List[float]): Начальная оценка позы второго скана
        max_iters (int, optional): Максимальное количество итераций алгоритма. Defaults to 20.
        tol (_type_, optional): Порог сходимости. Defaults to 1e-4.

    Returns:
        Tuple: Кортеж где:
            pose — найденная поза второго скана в глобальной системе координат
            pose_delta — разница между начальной и найденной позой
    """
    pts2_local, mask = simulation_laser.scanToPointsLocal(ranges2, angles2)     # преобразование второго скана в локальные точки
    
    pose = np.array(init_pose2)         # текущая оценка позы второго скана
    for it in range(max_iters):
        pts2_world = utils_poses_and_points.transformPoints(pts2_local, pose)   # преобразование точек второго скана в глобальные координаты
        
        # поиск ближайших соседей
        inds, dists = nearestNeighbors(pts2_world, scan1_world_pts) # массив индексов ближайших точек, массив расстояний до ближайших точек
        matched = scan1_world_pts[inds]                             # массив точек из scan1_world_pts, соответствующих ближайшим соседям
        
        valid = dists < 0.5                                         # логическая маска, отбирает пары точек, расстояние между которыми меньше 0.5
        if valid.sum() < 3: break                                   # если количество валидных пар меньше 3, алгоритм прекращает работу (слишком мало данных для сопоставления)
        
        A = pts2_world[valid]                                       # отфильтрованные точки второго скана в глобальной системе координат.
        B = matched[valid]                                          # соответствующие им точки из первого скана
        R, t, angle = bestFitTransform(A, B)                        # матрица поворота, вектор сдвига (dx, dy), угол поворота
        
        dth = angle     # изменение угла
        dx, dy = t      # изменения координат
        pose = utils_poses_and_points.poseMul(np.array([dx, dy, dth]), pose)    # применение изменения позы (dx, dy, dth) к текущей позе pose, ее обновление 
        
        if np.hypot(dx, dy) < tol and abs(dth) < tol:   break                   # если изменения позы (dx, dy, dth) меньше порога tol, алгоритм останавливается (достигнута сходимость)
    
    #  вычисление разницы между начальной и найденной позой
    pose_delta = utils_poses_and_points.poseMul(utils_poses_and_points.poseInv(init_pose2), pose)
    return pose, pose_delta