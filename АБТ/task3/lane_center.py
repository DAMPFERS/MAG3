from gym_duckietown.tasks.task_solution import TaskSolution








class LaneCenterTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)

    def solve(self):
        from numpy import ascontiguousarray # преобразует наблюдение в массив NumPy для обработки
        from cv2 import  inRange, countNonZero  
        
        def getPixelsCount(img) -> int:
            """
            Находит все пиксели в изображении img, значения которых лежат в диапазоне от lowerb до upperb (в формате BGR)
            return:  количество жёлтых пикселей на изображении.
            """    
            yellow_pixels = inRange(src=img, lowerb=(160,140,0), upperb=(180, 170, 120))
            return countNonZero(yellow_pixels)      # подсчитывает количество ненулевых пикселей
        
        
        env = self.generated_task['env']
        obs, _, _, _ = env.step([0, 0]) # obs - изображение с камеры

        linear_velocity = 0
        angular_velocity = 0.5
        
        rotated_flag = False
        while (not rotated_flag):
            img = ascontiguousarray(obs)
            pixels =  getPixelsCount(img[60:120, 305:335])
            if pixels > 20:
                rotated_flag = True
            obs, _, _, _ = env.step([linear_velocity, angular_velocity])
            env.render()

        linear_velocity = 0.5
        angular_velocity = 0
        
        moved_flag = False
        while (not moved_flag):
            img = ascontiguousarray(obs)
            pixels =  getPixelsCount(img[300:480, 200:440])
            if pixels > 5000:
                moved_flag = True
            obs, _, _, _ = env.step([linear_velocity, angular_velocity])
            env.render()