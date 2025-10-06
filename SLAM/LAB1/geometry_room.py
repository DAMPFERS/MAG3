# Геометрия комнаты и пересечение луч-сегмент

import numpy as np


from typing import List, Tuple
Point = Tuple[float, float]
Polygon = List[Point]
Edge = Tuple[Point, Point]


def makeSkewedRectangle(w=10.0, h=5.0, cut=1.0) -> Polygon:
    """
    Возвращает список вершин полигона (в порядке обхода): прямоугольник от (0,0) до (w,h) с срезанным верхним правым углом.
    Порядок: нижний левый, нижний правый, правый край среза, точка среза, верхний левый
    
    Args:
        w (float): Длина. Defaults to 10.0.
        h (float): Высота. Defaults to 5.0.
        cut (float): Вырезанный угол. Defaults to 1.0.

    Returns:
        list: Массив вершин полигона в порядке обхода
    """
    
    pts = np.array([
        [0.0, 0.0],
        [w, 0.0],
        [w, h - cut],
        [w - cut, h],
        [0.0, h]
    ])
    return pts




def polygonEdges(poly: Polygon) -> List[Edge]:
    """
    Получение списка ребер

    Args:
        poly (Polygon): Список вершин

    Returns:
        list: List[Edge] Список ребер (ребро = кортеж 2х точек)
    """
    edges = []
    n = len(poly)
    for i in range(n):
        a = poly[i]
        b = poly[(i+1) % n]
        edges.append((a,b))
    return edges




def raySegmentIntersection(P: Point, r: List[float], A: Point, B: Point, eps=1e-9) -> float:
    """
    Решает P + t*r = A + u*(B-A)

    Args:
        P (Point): Точка-позиция сканера      (точка начала луча)
        r (List[float]): Тензор угла поворота (направление луча)
        A (Point): Первая точка ребра (точка А)
        B (Point): Вторая точка ребра (точка В)
        eps (_type_, optional): Точность. Defaults to 1e-9.

    Returns:
        (t = float, None): Возвращает t (расстояние вдоль луча r) при пересечении (t>=0 и 0<=u<=1), иначе None
        t — параметр, определяющий расстояние вдоль луча r
        0<=u<=1 — параметр, определяющий положение точки на отрезке AB (от 0 до 1)
    """
    
    
    
    
    """
    t·r - u·(B - A) = A - P
    s = B - A (вектор отрезка)
    rhs = A - P (правая часть уравнения)
    
    t·r - u·s = rhs ->
    
    t·r_x - u·s_x = rhs_x
    t·r_y - u·s_y = rhs_y
    ->
    
    | r_x   -s_x |   | t |     | rhs_x |
    | r_y   -s_y | · | u |  =  | rhs_y |
    
    M — матрица коэффициентов:
    | r_x   -s_x |
    | r_y   -s_y |
    
    sol — вектор неизвестных:
    | t |
    | u |
    
    rhs — вектор правой части:
    | rhs_x |
    | rhs_y |
    
    Решение:
    sol = (M)-1·rhs
    """
    s = B - A
    M = np.column_stack((r, -s)) # объединение одномерных массивов в столбцы в двумерный массив
    rhs = A - P
    
    det = np.linalg.det(M)
    if abs(det) < eps:  return None     # матрица вырождена, и система не имеет единственного решения
    
    sol = np.linalg.solve(M, rhs)       #  решение системы уравнений M * [t; u] = rhs
    t, u = sol[0], sol[1]
    
    if t >= 0 and -eps <= u <= 1 + eps:   return t
    
    return None



if __name__ == "__main__":
    pass