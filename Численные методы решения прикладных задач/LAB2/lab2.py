import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


def inverseMatrixMethod(A, b):
    '''
    Метод обратной матрицы
    Решение: x=(A-1)b
    '''
    A_inv = np.linalg.inv(A)
    x = np.dot(A_inv, b)
    return x


def cramerMethod(A, b):
    '''
    Метод Крамера 
    '''
    det_A = np.linalg.det(A)
    x = np.zeros_like(b, dtype=float)
    for i in range(len(b)):
        A_i = A.copy()
        A_i[:, i] = b
        det_A_i = np.linalg.det(A_i)
        x[i] = det_A_i / det_A
    return x




def checkConvergence(A):
    '''
    Проверка на сходимость
    Для сходимости итерационных методов необходимо, чтобы норма матрицы BBB была меньше 1.
    Для метода простых итераций: B=E - A, где E - единичная матрица.
    Для метода Гаусса-Зейделя: B=((D-L)-1)U, где A=L+D+U.
    '''
    
    # Для метода простых итераций
    B_simple = np.eye(len(A)) - A
    norm_simple = np.linalg.norm(B_simple, ord=np.inf)

    # Для метода Гаусса-Зейделя
    D = np.diag(np.diag(A))
    L = -np.tril(A, k=-1)
    U = -np.triu(A, k=1)
    B_gauss_seidel = np.dot(np.linalg.inv(D - L), U)
    norm_gauss_seidel = np.linalg.norm(B_gauss_seidel, ord=np.inf)

    return norm_simple < 1, norm_gauss_seidel < 1


def simpleIteration(A, b, e=0.01, max_iter=1000):
    '''
    Метод простых итераций
    Итерационная формула: x^(k+1)=B * x^(k) + c, где B=E - A, c = b
    '''
    n = len(b)
    B = np.eye(n) - A
    c = b
    x = np.zeros_like(b, dtype=float)
    iterations = 0
    errors = []

    for _ in range(max_iter):
        x_new = np.dot(B, x) + c
        error = np.linalg.norm(x_new - x, ord=np.inf)
        errors.append(error)
        if error < e:
            break
        x = x_new
        iterations += 1

    return x_new, iterations, errors


def gaussSeidel(A, b, e=0.01, max_iter=1000):
    '''
    Метод Гаусса-Зейделя
    '''
    n = len(b)
    x = np.zeros_like(b, dtype=float)
    iterations = 0
    errors = []

    for _ in range(max_iter):
        x_new = x.copy()
        for i in range(n):
            s1 = np.dot(A[i, :i], x_new[:i])
            s2 = np.dot(A[i, i+1:], x[i+1:])
            x_new[i] = (b[i] - s1 - s2) / A[i, i]

        error = np.linalg.norm(x_new - x, ord=np.inf)
        errors.append(error)
        if error < e:
            break
        x = x_new
        iterations += 1

    return x_new, iterations, error


def plotConvergence(errors_simple, errors_gauss_seidel):
    plt.figure(figsize=(10, 6))
    plt.plot(errors_simple, label="Простые итерации")
    plt.plot(errors_gauss_seidel, label="Гаусса-Зейделя")
    plt.yscale("log")
    plt.xlabel("Номер итерации")
    plt.ylabel("Погрешность (log)")
    plt.title("Сходимость итерационных методов")
    plt.legend()
    plt.grid()
    plt.show()





def printResults(name_method1, name_method2, x_simple, x_gauss_seidel, iter_simple, iter_gauss_seidel, e):
    '''
    Вывод результатов в таблицу
    '''
    data = {
        # "Метод": ["Обратная матрица", "Крамер", "Простые итерации", "Гаусса-Зейделя"],
        "Метод": [name_method1, name_method2],
        "x1": [ x_simple[0], x_gauss_seidel[0]],
        "x2": [x_simple[1], x_gauss_seidel[1]],
        "x3": [x_simple[2], x_gauss_seidel[2]],
        "x4": [x_simple[3], x_gauss_seidel[3]],
        "max": [
            np.max(x_simple),
            np.max(x_gauss_seidel)
        ],
        "e": [e, e],
        "Число итераций": [iter_simple, iter_gauss_seidel]
    }
    df = pd.DataFrame(data)
    print(df)



def relaxedSimpleIteration(A, b, omega=0.5, e=0.01, max_iter=1000):
    n = len(b)
    B = np.eye(n) - omega * A
    c = omega * b
    x = np.zeros_like(b, dtype=float)
    iterations = 0
    errors = []

    for _ in range(max_iter):
        x_new = np.dot(B, x) + c
        error = np.linalg.norm(x_new - x, ord=np.inf)
        errors.append(error)
        if error < e:
            break
        x = x_new
        iterations += 1

    return x_new, iterations, error



if __name__ == "__main__":
    # Матрица A (4x4)
    A = np.array([
        [13, -4, -4, -4],
        [-1,  4, -1, -1],
        [4,   4, 11,  2],
        [11, 11, 11, 34]
    ], dtype=float)

    # Вектор b (1x4)
    b = np.array([.1, .15, .2, .25], dtype=float)
    
    x_inverse = inverseMatrixMethod(A, b)
    x_cramer = cramerMethod(A, b)

    
    print("Решение методом обратной матрицы:")
    for i in range(len(x_inverse)):    
        print(f"x{i}: {x_inverse[i]:.6f}", end=' ')
    print(' ')
    
    print("Решение методом Крамера:")
    for i in range(len(x_cramer)):    
        print(f"x{i}: {x_cramer[i]:.6f}", end=' ')
    print(' ')
    
    
    # Проверка на сходимость
    convergence_simple, convergence_gauss_seidel = checkConvergence(A)
    print(f"Сходимость простых итераций: {convergence_simple}")
    print(f"Сходимость Гаусса-Зейделя: {convergence_gauss_seidel}")


    eps = [0.01, 0.001, 0.0001]
    x_simple_list = []
    iter_simple_list = []
    errors_simple_list = []
    x_gauss_seidel_list = [] 
    iter_gauss_seidel_list = []
    errors_gauss_seidel_list = []
    
    
    for ep in eps:
        
        # Решение итерационными методами
        if convergence_simple:
            x_simple, iter_simple, errors_simple = simpleIteration(A, b, e=ep)
        else:
            x_simple = [0] * 4
            iter_simple = 0
            errors_simple = 0
        
        if convergence_gauss_seidel:
            x_gauss_seidel, iter_gauss_seidel, errors_gauss_seidel = gaussSeidel(A, b, e=ep)

        x_simple_list.append(x_simple)
        iter_simple_list.append(iter_simple)
        errors_simple_list.append(errors_simple)
        x_gauss_seidel_list.append(x_gauss_seidel) 
        iter_gauss_seidel_list.append(iter_gauss_seidel)
        errors_gauss_seidel_list.append(errors_gauss_seidel)
    # Визуализация
    # plotConvergence(errors_simple, errors_gauss_seidel)

    # Вывод результатов
    for i in range(len(eps)):
        printResults("Простые итерации", "Гаусса-Зейделя", x_simple_list[i], x_gauss_seidel_list[i], iter_simple_list[i], iter_gauss_seidel_list[i], e=eps[i])
        print("errors_gauss_seidel ", errors_gauss_seidel_list[i])
        
    
