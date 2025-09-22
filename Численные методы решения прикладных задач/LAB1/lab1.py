def bisection_Method(func, a : float, b : float, eps : float) -> tuple:
    """
    Метод половинного деления для нахождения корня уравнения f(x) = 0.

    Args:
        func (function): Исследуемая функция
        a (float): граница интервала [a, b], на котором f(a)*f(b) < 0
        b (float): граница интервала [a, b], на котором f(a)*f(b) < 0
        eps (float): Требуемая точность
    Returns:
        tuple: корень и количество итерация
    """
    if func(a) * func(b) > 0: raise ValueError("Функция на концах интервала должна иметь разные знаки")
    if a > b: raise ValueError("Неправильно определены границы интервала")
    iterations = 0
    while (b - a) / 2 > eps:
        iterations += 1
        mid = (a + b) / 2
        if func(mid) == 0: return (mid, iterations) # нашли точный корень
        elif func(a) * func(mid) < 0: b = mid
        else: a = mid
    return ((a + b) / 2, iterations)




def newton_Method(func, df, x0 : float, eps : float, max_iter=1000) -> tuple:
    """
    Метод Ньютона для нахождения корня f(x) = 0.

    Args:
        func (function): функция
        df (function): производная функции
        x0 (float): начальное приближение
        eps (float): требуемая точность
        max_iter (int, optional): ограничение на число итераций

    Returns:
        tuple: корень и количество итерация
    """
    iterations = 0
    x = x0
    while iterations < max_iter:
        iterations += 1
        fx = func(x)
        dfx = df(x)
        if dfx == 0: raise ValueError("Производная равна нулю, метод Ньютона не применим")
        x_new  = x - fx / dfx
        if abs(x_new - x) < eps: return (x_new, iterations)
        x = x_new
    raise RuntimeError("Метод Ньютона не сошёлся за указанное число итераций")




def simple_Iteration_Method(phi, x0 : float, eps : float, max_iter=1000) -> tuple:
    """
    Метод простых итераций для решения уравнения x = phi(x).

    Args:
        phi (function): функция итерации
        x0 (float): начальное приближение
        eps (float): требуемая точность
        max_iter (int, optional): ограничение на число итераций. Defaults to 1000.

    Returns:
        tuple: корень и количество итерация
    """
    
    iterations = 0
    x = x0
    while iterations < max_iter:
        iterations += 1
        x_new = phi(x)
        if abs(x_new - x) < eps: return (x_new, iterations)
        x = x_new
    raise RuntimeError("Метод простых итераций не сошёлся за указанное число итераций")















if __name__ == "__main__":
    
    # 1 methon
    # f = lambda x: x**3 - 5.3*x**2 - 3.45*x
    
    # eps = [0.01, 0.001, 0.0001]
    # for e in eps:
    #     print(f"EPS: {e:.4f}")
    #     root, iters = bisection_Method(f, a=-2, b=-0.1, eps=e)
    #     print(f" Value: {root:.6f}, Iters: {iters}")
    #     root, iters = bisection_Method(f, a=-0.1, b=1, eps=e)
    #     print(f" Value: {root:.6f}, Iters: {iters}")
    #     root, iters = bisection_Method(f, a=5, b=6, eps=e)
    #     print(f" Value: {root:.6f}, Iters: {iters}")
    #     print(" ")
     
        
    # 2 method
    # f = lambda x: x**3 - 5.3*x**2 - 3.45*x
    # df = lambda x: 3*x**2 - 10.6*x - 3.45
    
    # eps = [0.01, 0.001, 0.0001]
    # for e in eps:
    #     print(f"EPS: {e:.4f}")
    #     root, iters = newton_Method(func=f, df=df, x0=-1.0, eps=e)
    #     print(f" Value: {root:.6f}, Iters: {iters}")
    #     root, iters = newton_Method(func=f, df=df, x0=1.0, eps=e)
    #     print(f" Value: {root:.6f}, Iters: {iters}")
    #     root, iters = newton_Method(func=f, df=df, x0=6.0, eps=e)
    #     print(f" Value: {root:.6f}, Iters: {iters}")
    #     print(" ")
        
        
    # 3 method
    
    
    
    
    
    
    phi_0 = lambda x: x - 0.001 * (x**3 - 5.3*x**2 - 3.45*x)
    phi_1 = lambda x: ((x**2 - 3.45*x) / 5.3)
    phi_2 = lambda x: (5.3*x**2 + 3.45*x)**(1/3)

  
    
    
    
    eps = [0.01, 0.001, 0.0001]
    for e in eps:
        print(f"EPS: {e:.4f}")
        root, iters = simple_Iteration_Method(phi=phi_0, x0=-0.8, eps=e)
        print(f" Value: {root:.6f}, Iters: {iters}")
        root, iters = simple_Iteration_Method(phi=phi_1, x0=1.0, eps=e)
        print(f" Value: {root:.6f}, Iters: {iters}")
        root, iters = simple_Iteration_Method(phi=phi_2, x0=6.0, eps=e)
        print(f" Value: {root:.6f}, Iters: {iters}")
        print(" ")