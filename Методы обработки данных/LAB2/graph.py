import matplotlib.pyplot as plt
import numpy as np
import lab2


N = 5
thetas = [
    [0.0313, 0.1137, 0.0070, 0.0029],
    [0.0022, 0.2, 0.0806, 0.0058],
    [0.135, 0.0017, 0.260, 0.0073],
    [0.0023, 0.0021, 0.0064, 0.0143],
    [0.0021, 0.1352, 0.1225, 0.0034],
]


# данные
t = np.linspace(0, 10, 100)
# y_true = np.sin(2 * np.pi * t)
y_true = np.sqrt(0.01 * t)
# y_true = np.log(0.01*t)

# веса
weights = lab2.normalize_weights([np.exp(-lab2.delta(t, y_true, th)) for th in thetas])
best = lab2.best_theta(thetas, weights)

# 1. Эталон и все частицы
plt.figure(figsize=(8,5))
plt.plot(t, y_true, 'k-', label="Эталонная функция")
for i, th in enumerate(thetas):
    plt.plot(t, lab2.model(t, th), alpha=0.5, label=f"Частица {i+1}")
plt.legend()
plt.title("Эталон и траектории частиц")
plt.show()

# 2. Гистограмма весов
plt.figure(figsize=(6,4))
plt.bar(range(len(weights)), weights)
plt.title("Нормализованные веса частиц")
plt.xlabel("Номер частицы")
plt.ylabel("Вес")
plt.show()

# 3. Лучшая частица
plt.figure(figsize=(8,5))
plt.plot(t, y_true, 'k-', label="Эталонная функция")
plt.plot(t, lab2.model(t, best), 'r--', lw=2, label="Лучшая частица")
plt.legend()
plt.title("Сравнение лучшей частицы с эталоном")
plt.show()
