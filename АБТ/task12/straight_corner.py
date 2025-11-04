from gym_duckietown.tasks.task_solution import TaskSolution



class StraightCornerTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)

    def solve(self):
        from cv2 import countNonZero, inRange
        from numpy import ascontiguousarray
        
        def isNearCrossRoad(img):
            """
            Функция для обнаружения перекрестка на изображении.
            Возвращает True, если перекресток обнаружен, иначе False.
            """
            # Ищем красные пиксели в нижней половине изображения
            pixels = countNonZero(inRange(src=img[320:480, 256:640], lowerb=(155, 0, 0), upperb=(255, 100, 100)))
            print("Красных пикселей: ", pixels)
            return pixels > 15000  # Порог для обнаружения перекрестка
        
        
        env = self.generated_task['env']
        obs, _, _, _ = env.step([0, 0])  # Получаем начальное наблюдение
        linear_velocity = 1.0  # Линейная скорость (м/с)
        angular_velocity = 0.0  # Угловая скорость (рад/с)
        onCrossRoad = False  # Флаг для обнаружения перекрестка

        print(f"Целевые координаты: {self.generated_task['target_coordinates'][-1]}")

        # Основной цикл управления
        while True:
            img = ascontiguousarray(obs)        # Преобразуем наблюдение в изображение

            # Проверяем, находится ли робот на перекрестке
            if isNearCrossRoad(img):
                onCrossRoad = True
                print("Перекресток обнаружен!")

            # Если робот на перекрестке, двигаемся прямо
            if onCrossRoad:
                for tick in range(27):      # Проезд перекрестка за 27 шагов
                    obs, _, _, _ = env.step([linear_velocity, angular_velocity])
                    env.render()
                break  # Завершаем задачу после проезда перекрестка

            # Если перекресток не обнаружен, продолжаем движение
            obs, _, _, _ = env.step([linear_velocity, angular_velocity])
            env.render()

        print("Mission complite!")