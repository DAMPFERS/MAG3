#   Утилиты для работы с позами и точками

import numpy as np
from math import cos, sin


from typing import List, Tuple
Point = Tuple[float, float]
Polygon = List[Point]
Edge = Tuple[Point, Point]


def wrapToPi(a: float) -> float:
    """
    Нормализация угла в диапазон от -pi до pi

    Args:
        a (float): Угол в радианах

    Returns:
        float: Угол в диапазоне [-pi; pi]
    """
    return (a + np.pi) % (2 * np.pi) - np.pi




def poseMul(p: Tuple[float, float, float], q: Tuple[float, float, float]) -> List:
    """
    Выполнение композиции (умножения) двух поз робота 
    (Вычисление результирующей позы, если сначала применить позу p, а затем позу q)

    Args:
        p (Tuple[float, float, float]): поза робота в виде (x, y, theta)
        q (Tuple[float, float, float]): поза робота в виде (x, y, theta)

    Returns:
        List[float, float, float]: Результирующая поза r = p * q в виде массива [xr, yr, tr]
    """
    x1, y1, th1 = p
    x2, y2, th2 = q
    
    # координаты (x2, y2) сначала поворачиваются на угол th1 (чтобы перейти в систему координат первой позы), а затем сдвигаются на (x1, y1)
    ca = cos(th1)
    sa = sin(th1)
    xr = x1 + ca * x2 - sa * y2     # x_rotated = cos(th1) * x2 - sin(th1) * y2
    yr = y1 + sa * x2 + ca * y2     # y_rotated = sin(th1) * x2 + cos(th1) * y2
    tr = wrapToPi(th1 + th2)        # результирующий угол, равный сумме углов th1 и th2, нормализованный в диапазон [−pi, pi]
    return np.array([xr, yr, tr])



def poseInv(p: Tuple) -> List:
    """
    Вычисление обратной позы (inverse pose) для заданной позы робота

    Args:
        p (Tuple[float, float, float]): поза робота в виде (x, y, th):
            x, y — координаты положения робота в глобальной системе координат
            th — угол ориентации робота (в радианах) относительно глобальной системы координат

    Returns:
        List[float, float, float]: Обратная поза в виде массива [xi, yi, -th]:
            xi, yi — координаты положения робота в локальной системе координат
            -th — обратный угол ориентации
    """
    
    x, y, th = p
    ca = cos(th)
    sa = sin(th)
    xi = -ca * x - sa * y
    yi = sa * x - ca * y
    return np.array([xi, yi, -th])



def transformPoints(points: List[Point], pose: List) -> List[Point]:
    """
    Преобразование Nx2 точек локальных координат в глобальные по позе [x,y,theta]

    Args:
        points (List[Point]): Массив точек в локальных координатах
        pose (List): Позиция (x, y, угол)
        x, y — координаты начала локальной системы в глобальной,
        theta — угол поворота локальной системы относительно глобальной (в радианах).

    Returns:
        List[Point]: Массив точек в глобальных координатах
    """
    if len(points) == 0: return np.array([])
    
    ca = cos(pose[2])
    sa = sin(pose[2])
    R = np.array([[ca, -sa], [sa, ca]]) # матрица поворота размером 2x2
    
    # Проверка формы массива точек
    if points.ndim == 1: points = points.reshape(-1, 2)
    
    pts = points.dot(R.T) + pose[:2] # поворот точек из локальной системы координат в глобальную плюс смещение
    return pts


if __name__ == "__main__":
    pass