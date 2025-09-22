import math

# Функция и её производная
def f(x):
    return x**3 - 5.3*x**2 - 3.45*x

def df(x):
    return 3*x**2 - 10.6*x - 3.45

# Подбор k: ищем k в сетке [-1,1] с шагом step, при котором max|1 - k f'(x)| < 1 на отрезке [a,b]
def find_k_for_interval(a, b, step=0.001):
    xs = [a + i*(b-a)/200 for i in range(201)]
    best = None
    # сначала грубая сетка, потом можно сузить
    k_values = [i*step for i in range(int(-1/step), int(1/step)+1)]
    for k in k_values:
        maxabs = max(abs(1 - k*df(x)) for x in xs)
        if maxabs < 1:
            best = k
            break
    return best

# Метод простых итераций с phi_k(x) = x - k*f(x)
def simple_iteration_relaxed(f, a, b, x0=None, eps=1e-4, max_iter=10000):
    # подбираем k автоматически
    k = find_k_for_interval(a, b, step=0.001)
    if k is None:
        raise ValueError("Не удалось найти k на отрезке для сжатия; попробуй сузить отрезок или уменьшить шаг поиска k.")
    phi = lambda x: x - k * f(x)
    if x0 is None:
        x = (a + b) / 2.0
    else:
        x = x0
    iterations = 0
    while iterations < max_iter:
        iterations += 1
        x_new = phi(x)
        if abs(x_new - x) < eps:
            return x_new, iterations, k
        x = x_new
    raise RuntimeError("Не сошлось за max_iter итераций")

# Примеры использования (для трёх окрестностей корней)
if __name__ == "__main__":
    intervals = [
        (-0.2, 0.2),   # окрестность корня 0
        (-0.8, -0.4),  # окрестность корня ~ -0.586
        (5.6, 6.0)     # окрестность корня ~ 5.886
    ]
    for a, b in intervals:
        try:
            root, iters, k = simple_iteration_relaxed(f, a, b, eps=1e5)
            print(f"Интервал [{a},{b}]: root ≈ {root:.6f}, итераций={iters}, k={k}")
        except Exception as e:
            print(f"Интервал [{a},{b}]: ошибка: {e}")
            
