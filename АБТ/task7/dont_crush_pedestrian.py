from gym_duckietown.tasks.task_solution import TaskSolution




class DontCrushPedestrianTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)

    def solve(self):
        from cv2 import countNonZero, inRange
        from numpy import array, ascontiguousarray, linalg
        
        def  detectPedestrians(img):
            pixels = countNonZero(inRange(src=img, lowerb=(150, 150, 0), upperb=(255, 255, 150)))
            print("yellow pixels: ", pixels)
            return pixels > 15000
        
        
        env = self.generated_task['env']
        obs, _, _, _ = env.step([0, 0])  # Получаем начальное наблюдение
        target_coordinates = self.generated_task['target_coordinates'][-1]  # Целевые координаты
        print(f"Целевые координаты: {target_coordinates}")

        # Параметры управления
        linear_velocity = 0.5   # Линейная скорость (м/с)
        angular_velocity = 0.0  # Угловая скорость (рад/с)

        # Основной цикл управления
        while True:
            # Преобразуем наблюдение в изображение
            img = ascontiguousarray(obs)

            # Проверяем, есть ли пешеходы на пути
            if detectPedestrians(img):      linear_velocity = 0.0  
            else:                           linear_velocity = 0.5 

            obs, _, _, _ = env.step([linear_velocity, angular_velocity])
            env.render()

            # Получаем текущие координаты робота
            current_position = env.unwrapped.cur_pos
            print(f"coord: {current_position}")

            #  достигнуты ли целевые координаты
            distance_to_target = linalg.norm(array(current_position) - array(target_coordinates))
            if distance_to_target < 0.1:  # Порог для завершения задачи
                print("Mission complete!")
                break