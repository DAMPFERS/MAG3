#   Утилиты для работы с позами и точками

import numpy as np
from math import cos, sin


from typing import List, Tuple
Point = Tuple[float, float]
Polygon = List[Point]
Edge = Tuple[Point, Point]


def wrapToPi(a: float) -> float:
    return (a + np.pi) % (2 * np.pi) - np.pi



def poseMul(p: tuple, q: tuple) -> list:
    """Принимает позы p и q (both [x,y,theta])
    возвращает позу r = p * q"""
    x1, y1, th1 = p
    x2, y2, th2 = q
    ca = cos(th1)
    sa = sin(th1)
    xr = x1 + ca * x2 - sa * y2
    yr = y1 + sa * x2 - sa * y2
    tr = wrapToPi(th1 + th2)
    return np.array([xr, yr, tr])


def poseInv(p: tuple) -> list:
    x, y, th = p
    ca = cos(th)
    sa = sin(th)
    xi = -ca * x - sa * y
    yi = sa * x - ca * y
    return np.array([xi, yi, -th])



def transformPoints(points: List[Point], pose: List) -> List[Point]:
    """
    Преобразование Nx2 точек по позе [x,y,theta]

    Args:
        points (List[Point]): Массив точек в локальных координатах
        pose (List): Позиция (x, y, угол)
        x, y — координаты начала локальной системы в глобальной,
        theta — угол поворота локальной системы относительно глобальной (в радианах).

    Returns:
        List[Point]: Массив точек в глобальных координатах
    """
    ca = cos(pose[2])
    sa = sin(pose[2])
    R = np.array([[ca, -sa], [sa, ca]]) # матрица поворота размером 2x2
    pts = points.dot(R.T) + pose[:2] # поворот точек из локальной системы координат в глобальную плюс смещение
    return pts


if __name__ == "__main__":
    pass