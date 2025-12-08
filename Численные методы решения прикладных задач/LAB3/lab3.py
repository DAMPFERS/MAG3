import numpy as np
import matplotlib.pyplot as plt

# Задание функции и параметров
def g(x):
    return np.sin(x) - 3 * np.cos(2 * x)


def main():
    a = np.pi / 4
    b = np.pi / 2
    N_values = [10, 20]

    for N in N_values:
        print(f"\n--- N = {N} ---")

        # 1. Узлы интерполяции и значения функции
        h = (b - a) / N
        x = np.array([a + h * i for i in range(N + 1)])
        y = g(x)

        print("Вектор x:", x)
        print("Вектор y:", y)

        # 2. Построение графика
        plt.figure(figsize=(10, 5))
        plt.plot(x, y, 'o-', label=f'g(x), N={N}')
        plt.title(f"График функции g(x) при N={N}")
        plt.xlabel("x")
        plt.ylabel("g(x)")
        plt.legend()
        plt.grid()
        plt.show()

        # 3. Задание точки z
        z = (a + b) / 2  # Произвольная точка внутри отрезка
        z += z * 0.1
        while z in x:  # Убедимся, что z не совпадает с узлами
            z += 0.01

        # Кусочно-постоянная интерполяция (слева и справа)
        index = np.searchsorted(x, z) - 1
        F_left = y[index]
        F_right = y[index + 1]

        # Кусочно-линейная интерполяция
        k = (y[index + 1] - y[index]) / (x[index + 1] - x[index])
        l = y[index] - k * x[index]
        F_linear = k * z + l

        # Погрешности
        error_left = abs(g(z) - F_left)
        error_right = abs(g(z) - F_right)
        error_linear = abs(g(z) - F_linear)

        print(f"\nТочка z: {z:.4f}")
        print(f"g(x): {g(z):.4f}")
        print(f"Кусочно-постоянная слева: F(z) = {F_left:.4f}, погрешность = {error_left:.4f}")
        print(f"Кусочно-постоянная справа: F(z) = {F_right:.4f}, погрешность = {error_right:.4f}")
        print(f"Кусочно-линейная: F(z) = {F_linear:.4f}, погрешность = {error_linear:.4f}")
        print(f"Уравнение прямой: F(x) = {k:.4f}x + {l:.4f}")

        # 4. Интерполяция полиномом Лагранжа
        def lagrange_interpolation(x_points, y_points, z):
            n = len(x_points)
            result = 0.0
            for i in range(n):
                term = y_points[i]
                for j in range(n):
                    if i != j:
                        term *= (z - x_points[j]) / (x_points[i] - x_points[j])
                result += term
            return result

        F_lagrange = lagrange_interpolation(x, y, z)
        error_lagrange = abs(g(z) - F_lagrange)

        print(f"\nИнтерполяция Лагранжа: F(z) = {F_lagrange:.4f}, погрешность = {error_lagrange}")


if __name__ == "__main__":
    main()