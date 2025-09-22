import numpy as np
import random
import matplotlib.pyplot as plt
import math

def generate_Noisy_Linear_Data(a, b, data_size, noise_sigma, random_state):
    x = np.arange(0, data_size, 1.)
    mu = 0.0
    random.seed(random_state)
    noise = np.empty((data_size, 1))
    y = np.empty((data_size, 1))
    for i in range(data_size):
        noise[i] = random.gauss(mu, noise_sigma)
        y[i] = b + a*x[i] + noise[i]
    return x, y




def predict(a, b, xi):
    return [a * val + b for val in xi]
    




def solution(x, y):
    # Создаём матрицу A: первый столбец — x, второй — единицы
    A = np.column_stack((x, np.ones(len(x))))
    B = np.array(y)

    # Вычисляем A^T * A
    ATA = np.dot(A.T, A)
    # Вычисляем обратную матрицу (ATA)^{-1}
    ATA_inv = np.linalg.inv(ATA)
    # Вычисляем (ATA)^{-1} * A^T
    ATA_inv_AT = np.dot(ATA_inv, A.T)
    # Вычисляем коэффициенты: (ATA)^{-1} * A^T * B
    res = np.dot(ATA_inv_AT, B)
    
    return res
    
    
def main_Task1():
    x_linear, y_linear = generate_Noisy_Linear_Data(4, -10, 10, 10, 42)
    
    
    a, b = solution(x_linear, y_linear)
    print(a, b)
    
    y_linear_pred = predict(a, b, x_linear)
    

    plt.plot(x_linear, y_linear, 'o', label = "Истинные значения")
    plt.plot(x_linear, y_linear_pred, label = "Расчетные значения") 
    
    plt.legend(loc = "best", fontsize=12)
    plt.xlabel('x (порядковый номер измерения)', fontsize=14)
    plt.ylabel('y (Значение измерения)', fontsize=14)
    
    plt.show()




def main_Task2():
    x = [-0.5, 0.81, 0.99, 0.6, 5., 2.9, 1.4, 3.2, 0.01, 2.13]
    y = [-3.67, 2.99, 1.83, 3.95, 2.11, 4.15, 0.23, 2.22, 1.01, 4.56]
    
    # Объединяем x и y в список кортежей
    points = list(zip(x, y))

    # Сортируем по первому элементу кортежа (координате x)
    points_sorted = sorted(points, key=lambda point: point[0])

    # Разделяем обратно на x_sorted и y_sorted
    x, y = zip(*points_sorted)

    y_porabol = [i*i + 1 for i in x]
    
    res = 0
    for i in range(len(x)):
        res += (y_porabol[i] - y[i])**2
    res = math.sqrt(res)
    print(res)
    
    plt.plot(x, y, 'o', label = "Набор точек")
    plt.plot(x, y_porabol, label = "Порабола")  
    plt.legend(loc = "best", fontsize=12)
    plt.xlabel('x (порядковый номер измерения)', fontsize=14)
    plt.ylabel('y (Значение измерения)', fontsize=14)
    
    plt.show()
    pass








if __name__ == "__main__":

    main_Task1()
    # main_Task2()
    
    pass

