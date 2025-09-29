import numpy as np
from typing import List, Tuple, Union

def model(x: np.ndarray, theta: List[float]) -> np.ndarray:
    c0, c1, c2, c3 = theta
    return c0 * np.exp(c1 * x + c2) + c3

def delta(t: Union[np.ndarray, List[float]],
          y: Union[np.ndarray, List[float]],
          theta: List[float]) -> float:
    t = np.asarray(t, dtype=float)
    y = np.asarray(y, dtype=float)
    y_pred = model(t, theta)
    return float(np.sum((y - y_pred) ** 2))

def normalize_weights(weights: List[float]) -> np.ndarray:
    """Стабильная нормализация через вычитание максимума из лог-весов:
       вход - не обязательно уже нормализованные веса (мы ожидаем лог-вес = -delta),
       но функция работает и для положительных чисел."""
    w = np.asarray(weights, dtype=float)
    # если суммы почти ноль — защитимся
    if np.all(w == 0):
        return np.ones_like(w) / len(w)
    s = w.sum()
    if s == 0 or not np.isfinite(s):
        # если суммы нет (все нули или inf/NaN), сделаем равномерное распределение
        return np.ones_like(w) / len(w)
    return (w / s)

def best_theta(thetas: List[List[float]], weights: List[float]) -> List[float]:
    idx = int(np.argmax(weights))
    return thetas[idx]

def solution(N: int,
             thetas: List[List[float]],
             t: np.ndarray = None,
             y: np.ndarray = None,
             return_weight: bool = False) -> Union[List[float], float]:
    """
    N, thetas - входные данные.
    Если t,y не заданы — используем тестовую траекторию sin(2πt).
    return_weight=False => возвращаем theta (вектор).
    return_weight=True  => возвращаем нормализованный вес лучшей частицы (скаляр).
    """
    # тестовые данные, если не заданы (можешь заменить на реальные)
    if t is None or y is None:
        t = np.linspace(1.0, 10.0, 100)
        # y = np.sin(2 * np.pi * t)
        y = np.sqrt(0.01*t)

    # считаем ошибки (delta) для каждой частицы
    deltas = np.array([delta(t, y, th) for th in thetas])

    # переводим в лог-веса (чем меньше ошибка, тем больше лог-вага)
    logw = -deltas

    # стабильная эксп-нормализация: вычтем максимум, чтобы избежать underflow
    logw = logw - np.max(logw)
    w = np.exp(logw)

    # нормализуем
    w = compute_weights(t, y, thetas)
    # w = normalize_weights(w)

    # индекс и возвращаемое значение
    best_idx = int(np.argmax(w))
    if return_weight:
        return float(w[best_idx])
    return thetas[best_idx]



def compute_weights(t, y_true, thetas):
    deltas = np.array([delta(t, y_true, th) for th in thetas])
    
    # логарифм весов
    logw = -deltas
    
    # стабилизация: вычтем максимум
    logw -= np.max(logw)
    
    w = np.exp(logw)  # теперь значения не улетят в 0
    w = normalize_weights(w)
    return w




# Пример запуска
if __name__ == "__main__":
    N = 5
    thetas = [
        [0.0313, 0.1137, 0.0070, 0.0029],
        [0.0022, 0.2, 0.0806, 0.0058],
        [0.135, 0.0017, 0.260, 0.0073],
        [0.0023, 0.0021, 0.0064, 0.0143],
        [0.0021, 0.1352, 0.1225, 0.0034],
    ]

    best_theta_vec = solution(N, thetas, return_weight=False)
    best_weight = solution(N, thetas, return_weight=True)

    print("best theta:", best_theta_vec)
    print("best normalized weight:", best_weight)
