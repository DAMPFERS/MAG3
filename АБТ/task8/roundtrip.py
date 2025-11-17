from gym_duckietown.tasks.task_solution import TaskSolution


class RoundtripTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)

    def solve(self):
        
        import numpy as np
        
        env = self.generated_task['env']
        
        # Инициализация
        obs, _, _, _ = env.step([0, 0])
        env.render()
        
        # Получаем стартовую позицию и все целевые координаты
        start_pos = env.cur_pos.copy()
        target_coordinates = self.generated_task['target_coordinates']
        
        print(f"Стартовая позиция: {start_pos}")
        print(f"Все целевые точки: {target_coordinates}")
        
        # Функция нормализации угла
        def normalize_angle(angle):
            while angle > np.pi:
                angle -= 2 * np.pi
            while angle < -np.pi:
                angle += 2 * np.pi
            return angle

        # Посещаем все три целевые точки
        for i, target in enumerate(target_coordinates):
            target_pos = np.array(target)
            print(f"Движение к целевой точке {i+1} ")
            
            # Движение к целевой точке
            current_target = target_pos
            tolerance = 0.1
            max_steps = 1000
            step_count = 0
            
            while step_count < max_steps:
                current_pos = env.cur_pos
                current_angle = env.cur_angle
                
                # Вычисляем вектор направления
                direction = current_target - current_pos
                distance = np.linalg.norm(direction)
                
                # Проверяем достижение цели
                if distance < tolerance:
                    print(f"Достигнута целевая точка {i+1}")
                    break
                
                # Вычисляем целевой угол
                target_angle = np.arctan2(direction[1], direction[0])
                angle_diff = normalize_angle(target_angle - current_angle)
                
                # Пропорциональный регулятор
                kp_angular = 2.0
                angular_velocity = kp_angular * angle_diff
                angular_velocity = np.clip(angular_velocity, -1.0, 1.0)
                
                # Линейная скорость зависит от угловой ошибки
                if abs(angle_diff) > 0.3:
                    linear_velocity = 0.1  # Медленное движение при большом угле
                else:
                    linear_velocity = 0.3  # Нормальное движение
                
                # Выполняем шаг
                obs, reward, done, info = env.step([linear_velocity, angular_velocity])
                env.render()
                
                step_count += 1
                
                if step_count % 100 == 0:
                    print(f"  Шаг {step_count}, расстояние: {distance:.3f}")
            
            # ПОЛНАЯ ОСТАНОВКА после достижения целевой точки
            print("Полная остановка...")
            for _ in range(10):
                obs, _, _, _ = env.step([0, 0])
                env.render()
            
            # РАЗВОРОТ НА 180 ГРАДУСОВ (линейная скорость = 0)
            if i < len(target_coordinates) - 1:
                print("Разворот на 180 градусов...")
                target_angle = normalize_angle(env.cur_angle + np.pi)
                max_turn_steps = 200
                turn_step = 0
                
                while turn_step < max_turn_steps:
                    current_angle = env.cur_angle
                    angle_diff = normalize_angle(target_angle - current_angle)
                    
                    if abs(angle_diff) < 0.1:
                        print("Разворот завершен")
                        break
                    
                    # ПОВОРОТ НА МЕСТЕ - линейная скорость = 0
                    angular_velocity = 0.8 if angle_diff > 0 else -0.8
                    obs, _, _, _ = env.step([0.0, angular_velocity])  # linear_velocity = 0
                    env.render()
                    
                    turn_step += 1
                    
                    if turn_step % 20 == 0:
                        print(f"  Поворот: шаг {turn_step}, разница углов: {angle_diff:.3f}")
                
                # ПОЛНАЯ ОСТАНОВКА после разворота

                for _ in range(10):
                    obs, _, _, _ = env.step([0, 0])
                    env.render()
        
        # Движение обратно в стартовую точку
        print("Возврат в стартовую позицию")
        current_target = start_pos
        tolerance = 0.1
        max_steps = 1000
        step_count = 0
        
        while step_count < max_steps:
            current_pos = env.cur_pos
            current_angle = env.cur_angle
            
            # Вычисляем вектор направления
            direction = current_target - current_pos
            distance = np.linalg.norm(direction)
            
            # Проверяем достижение цели
            if distance < tolerance:
                print("Возврат в стартовую позицию завершен!")
                break
            
            # Вычисляем целевой угол
            target_angle = np.arctan2(direction[1], direction[0])
            angle_diff = normalize_angle(target_angle - current_angle)
            
            # Пропорциональный регулятор
            kp_angular = 2.0
            angular_velocity = kp_angular * angle_diff
            angular_velocity = np.clip(angular_velocity, -1.0, 1.0)
            
            # Линейная скорость зависит от угловой ошибки
            if abs(angle_diff) > 0.3:
                linear_velocity = 0.1  # Медленное движение при большом угле
            else:
                linear_velocity = 0.3  # Нормальное движение
            
            # Выполняем шаг
            obs, reward, done, info = env.step([linear_velocity, angular_velocity])
            env.render()
            
            step_count += 1
            
            if step_count % 100 == 0:
                print(f"  Шаг {step_count}, расстояние: {distance:.3f}")
        

        for _ in range(30):
            obs, _, _, _ = env.step([0, 0])
            env.render()
    
    
    
    
    
    
    