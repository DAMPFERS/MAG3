from gym_duckietown.tasks.task_solution import TaskSolution


class RandomMapTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)

    def solve(self):
        import numpy as np
        import cv2
        
        def detectLaneOffset(obs):
            """
            Возвращает:
            - error: смещение линии относительно центра
            - found: True/False, видна ли линия вообще
            """
            # Нижняя часть изображения
            roi = obs[250:480, :]
            # roi = obs[:250, :]

            mask = cv2.inRange(src=roi, lowerb=(160,140,0), upperb=(180, 170, 120))
            # Моменты
            M = cv2.moments(mask)
            # print(M["m00"])
            if M["m00"] < 5_000:
                return 0, False  # линия не найдена

            cx = int(M["m10"] / M["m00"])    # центр жёлтой линии
            img_center = roi.shape[1] // 2   # центр картинки
            error = cx - img_center          # >0 линия справа нужно поворачивать вправо
            # print("error ", error)
            return error, True
        
        def laneFollowStep(env):
            obs, _, _, _ = env.step([0, 0])
            error, found = detectLaneOffset(obs)

            if not found:
                # если потеряли линию → медленный разворот
                return [0.1, 1.0]

            # простой P-контроллер
            Kp = 0.004
            angular = -Kp * error

            # скорость снижается при большом отклонении
            linear = max(0.1, 0.45 - abs(angular))

            return [linear, angular]    
    
        env = self.generated_task["env"]

        target_coordinates = self.generated_task["target_coordinates"][-1]
        print("target_coordinates ", target_coordinates)
        
        
        goal = np.array([target_coordinates[0], target_coordinates[2]])

        # стартовая позиция
        start_pos = np.array([env.cur_pos[0], env.cur_pos[2]])
        print("start_pos", start_pos)
        
        print("1: движение к цели по полосе движения")
        for _ in range(35):   # поворот фиксированного количества шагов
            obs, _, _, _ = env.step([0, 1.0])
            env.render()
        while True:
            pos = np.array([env.cur_pos[0], env.cur_pos[2]])
            dist = np.linalg.norm(pos - goal)

            if dist < 0.40:   # достигли точки
                break

            action = laneFollowStep(env)
            obs, _, _, _ = env.step(action)
            env.render()
            
        print(" 2: Разворот")

        for _ in range(65):   # поворот фиксированного количества шагов
            obs, _, _, _ = env.step([0, 1.0])
            env.render()
            
        print("Возврат")

        while True:
            pos = np.array([env.cur_pos[0], env.cur_pos[2]])
            dist = np.linalg.norm(pos - start_pos)

            if dist < 0.20:
                break

            action = laneFollowStep(env)
            obs, _, _, _ = env.step(action)
            env.render()

        print("СТОП")
    
    
    