from gym_duckietown.tasks.task_solution import TaskSolution



class Ride3MDuckiebotTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)

    def solve(self):
        from time import time
        from cv2 import countNonZero, inRange
        from numpy import ascontiguousarray
        
        def duckieBot(img):
            """
            Функция для обнаружения машинки (duckiebot) на изображении.
            Возвращает количество красных пикселей.
            """
            # Ищем красные пиксели в нижней половине изображения
            red_pixels = countNonZero(inRange(src=img, lowerb=(210, 0, 0), upperb=(255, 80, 80)))
            print("Красных пикселей: ", red_pixels)
            return red_pixels
        
        
        
        env = self.generated_task['env']
        # target_coordinates = self.generated_task['target_coordinates'][-1]
        obs, _, _, _ = env.step([0, 0])  # Получаем начальное наблюдение
        env.render()  # Рендерим среду

        # Получаем начальную позицию
        start_pos = env.cur_pos.copy()
        print(f"Стартовая позиция: {start_pos}")

        linear_velocity = 0.0     
        angular_velocity = 0.0    
        distance_to_travel = 3.0  # Расстояние, которое нужно проехать (метры)
        distance_traveled = 0.0   # Пройденное расстояние

        # Начальное время
        start_time = time()
        t0 = start_time

        while distance_traveled < distance_to_travel:
            # Преобразуем наблюдение в изображение
            img = ascontiguousarray(obs)

            # Проверяем, есть ли машинка на пути
            red_pixels = duckieBot(img)
            if red_pixels > 5000:       linear_velocity = 0.0  # Останавка
            else:                       linear_velocity = 0.4 

            obs, _, _, _ = env.step([linear_velocity, angular_velocity])
            env.render()


            
             # Вычисляем фактическое пройденное расстояние
            current_pos = env.cur_pos
            # Вычисляем расстояние по оси X (направление движения)
            distance_traveled = abs(current_pos[0] - start_pos[0])
            
            print(f"distance: {distance_traveled:.2f} m, pos: {current_pos}")
            
            
        print("СТОП")
        for _ in range(10):
            obs, _, _, _ = env.step([0, 0])
            env.render()


