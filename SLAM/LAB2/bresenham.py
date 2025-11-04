import numpy as np
from typing import List, Tuple

def bresenhamLine(start: Tuple[int, int], end: Tuple[int, int]) -> List[Tuple[int, int]]:
    """
    Алгоритм Брезенхема для построения линии между двумя точками в сетке
    
    Args:
        start: начальная точка (x0, y0)
        end: конечная точка (x1, y1)
    
    Returns:
        Список точек (координат ячеек), составляющих линию
    """
    x0, y0 = start
    x1, y1 = end
    points = []
    
    dx = abs(x1 - x0)
    dy = abs(y1 - y0)
    
    x, y = x0, y0
    
    x_step = 1 if x1 > x0 else -1
    y_step = 1 if y1 > y0 else -1
    
    if dx > dy:
        err = dx / 2.0
        while x != x1:
            points.append((x, y))
            err -= dy
            if err < 0:
                y += y_step
                err += dx
            x += x_step
            
    else:
        err = dy / 2.0
        while y != y1:
            points.append((x, y))
            err -= dx
            if err < 0:
                x += x_step
                err += dy
            y += y_step
            
    points.append((x, y))
    return points