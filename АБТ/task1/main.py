from gym_duckietown.tasks.task_solution import TaskSolution


class DefaultTaskSolution(TaskSolution):
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
            red_pixels = countNonZero(inRange(img[240:480, 0:640], (210, 0, 0), (255, 80, 80)))
            print("Красных пикселей: ", red_pixels)
            return red_pixels
        
        
        print("0_o")
        env = self.generated_task['env']
        # target_coordinates = self.generated_task['target_coordinates'][-1]
        obs, _, _, _ = env.step([0, 0])  # Получаем начальное наблюдение
        env.render()  # Рендерим среду

        # Параметры управления
        linear_velocity = 0.0     
        angular_velocity = 0.0    
        distance_to_travel = 3.0  # Расстояние, которое нужно проехать (метры)
        distance_traveled = 0.0   # Пройденное расстояние

        # Начальное время
        start_time = time()


        while distance_traveled < distance_to_travel:
            # Преобразуем наблюдение в изображение
            img = ascontiguousarray(obs)

            # Проверяем, есть ли машинка на пути
            red_pixels = duckieBot(img)
            if red_pixels > 700:    linear_velocity = 0.0  # Останавка
            else:                   linear_velocity = 0.0 

            # Шаг 
            obs, _, _, _ = env.step([linear_velocity, angular_velocity])
            env.render()

            # Обновление пройденного расстояния
            current_time = time()
            time_elapsed = current_time - start_time
            distance_traveled = linear_velocity * time_elapsed
            
            print(f"Пройдено: {distance_traveled:.2f} м")
        print("Задача завершена: пройдено 3 метра!")
            



if __name__ == "__main__":
    # код ниже требуется для возможности запуска вашего решения в описываемом образе, при отправки решения в систему проверки данный код не требуется
    from gym_duckietown.tasks.default.task_generator import DefaultTaskGenerator

    task_generator = DefaultTaskGenerator()
    task_generator.generate_task()
    solution = DefaultTaskSolution(task_generator.generated_task)
    solution.solve()


