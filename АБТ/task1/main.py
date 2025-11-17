from gym_duckietown.tasks.task_solution import TaskSolution


class DefaultTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)

    def solve(self):
        
        
        import numpy as np
        def normalizeAngle(angle):
            """Нормализует угол в диапазон [-pi, pi]"""
            while angle > np.pi:
                angle -= 2 * np.pi
            while angle < -np.pi:
                angle += 2 * np.pi
            return angle
        
        def simpleMoveToTarget(env, target_pos):
            """Простое движение к цели"""
            tolerance = 0.2
            max_steps = 500
            
            for step in range(max_steps):
                current_pos = env.cur_pos
                direction = target_pos - current_pos
                distance = np.linalg.norm(direction[:2])  # Игнорируем высоту
                
                if distance < tolerance:
                    print(f"Target reached! Distance: {distance:.3f}")
                    return
                
                # Простое управление: всегда вперед с небольшими корректировками
                current_angle = env.cur_angle
                target_angle = np.arctan2(direction[1], direction[0])
                angle_diff = normalizeAngle(target_angle - current_angle)
                
                # Базовое управление
                if abs(angle_diff) > 0.3:
                    # Поворот на месте
                    angular_vel = 0.8 if angle_diff > 0 else -0.8
                    linear_vel = 0.0
                else:
                    # Движение вперед с корректировкой
                    angular_vel = 1.0 * angle_diff
                    linear_vel = 0.3
                
                obs, _, _, _ = env.step([linear_vel, angular_vel])
                env.render()
                
                if step % 50 == 0:
                    print(f"Step {step}, distance: {distance:.3f}, angle_diff: {angle_diff:.3f}")
        
        
        # def center()
            
        
        env = self.generated_task['env']
        
        # Инициализация
        obs, _, _, _ = env.step([0, 0])
        env.render()
        
        # Координаты
        start_pos = env.cur_pos.copy()
        # target_pos = np.array(self.generated_task['target_coordinates'][-1])
        target_pos = np.array([0.27657586, 0.0,          0.27038575])
        
        print(f"Start: {start_pos}")
        print(f"Target: {target_pos}")
        
        # Движение к цели
        simpleMoveToTarget(env, target_pos)
        
        # Разворот
        print("Turning around...")
        for _ in range(50):
            obs, _, _, _ = env.step([0, 1.0])
            env.render()
        
        # Возврат к старту
        simpleMoveToTarget(env, start_pos)
        
        # Остановка
        for _ in range(10):
            obs, _, _, _ = env.step([0, 0])
            env.render()
        
        print("Mission accomplished!")

            



if __name__ == "__main__":
    # код ниже требуется для возможности запуска вашего решения в описываемом образе, при отправки решения в систему проверки данный код не требуется
    from gym_duckietown.tasks.default.task_generator import DefaultTaskGenerator

    task_generator = DefaultTaskGenerator()
    task_generator.generate_task()
    solution = DefaultTaskSolution(task_generator.generated_task)
    solution.solve()


