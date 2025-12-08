from gym_duckietown.tasks.task_solution import TaskSolution
import numpy as np
import cv2
import math
import time


class RandomMapAggressiveTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)
        
        self.last_robot_check = 0

    
    def detectRoadLine(self, obs):
        obs2 = obs[250:480, 0:640]
        color_start_range = (150, 150, 0)
        color_end_range = (255, 255, 150)
        color_filter_mask = cv2.inRange(
            src=obs2,
            lowerb=color_start_range,
            upperb=color_end_range
        )
        M = cv2.moments(color_filter_mask)
        if M["m00"] < 8000:
            return 0
        cx = int(M["m10"] / M["m00"])
        target_cx = obs2.shape[1] // 2 - 120
        offset = cx - target_cx
        return offset
    
    
    def detectOtherRobots(self, obs):
        robots = []
        height, width = obs.shape[:2]
        hsv = cv2.cvtColor(obs, cv2.COLOR_BGR2HSV)
        
        color_ranges = [
            ((0, 100, 100), (10, 255, 255)),
            ((100, 100, 100), (130, 255, 255)),
            ((35, 100, 100), (85, 255, 255)),
            ((160, 100, 100), (180, 255, 255)),
        ]
        
        for (lower, upper) in color_ranges:
            mask = cv2.inRange(hsv, np.array(lower), np.array(upper))
            contours, _ = cv2.findContours(mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            
            for contour in contours:
                if cv2.contourArea(contour) > 100:
                    M = cv2.moments(contour)
                    if M["m00"] > 0:
                        cx = int(M["m10"] / M["m00"])
                        cy = int(M["m01"] / M["m00"])
                        distance = (height - cy) / height * 2.0
                        if distance < 0.4 and abs(cx - width//2) < 100:
                            robots.append({"distance": distance})
        
        return robots
    
    
    
        # Объезд робота с левой стороны
    def avoidRobot(self, env, robots):
        print("Объезд ")
        for _ in range(15):
            env.step([0.2, 0.6])
            env.render()
        for _ in range(20):
            env.step([0.3, 0.0])
            env.render()
        for _ in range(20):
            env.step([0.2, -0.6])
            env.render()
        print("Объезд конец")
        
        
    def initializeCar(self, env):
        for i in range(80):
            line = env.get_lane_pos2(env.cur_pos, env.cur_angle)
            
            # Простой П-регулятор
            angular_velocity = line.angle_rad * 3.0 - line.dist * 4.0
            
            if abs(angular_velocity) < 0.4:  # Минимум 0.4!
                angular_velocity = 0.4 if angular_velocity > 0 else -0.4
            
            angular_velocity = max(-1.0, min(1.0, angular_velocity))
            
            # Всегда двигаемся вперед
            env.step([0.12, angular_velocity])
            env.render()
            
            if i % 20 == 0:
                print(f"Шаг {i}: steering={angular_velocity:.2f}")
            
            if abs(line.dist) < 0.03 and abs(line.angle_rad) < math.radians(10):
                print(f"Выровнено за {i} шагов")
                return True
        #
        print("Выравнивание было")
        return True
    
    
    # -------------------------------------------------------
    # 1. Детекция смещения от жёлтой линии (PID)
    # -------------------------------------------------------
    def detectLaneOffset(self, obs):
        roi = obs[250:480, :]
        mask = cv2.inRange(roi, (160,140,0), (180,170,120))

        M = cv2.moments(mask)
        if M["m00"] < 4000:
            return 0, False

        cx = int(M["m10"] / M["m00"])
        img_center = roi.shape[1] // 2
        error = cx - img_center
        return error, True

    # -------------------------------------------------------
    # 2. Детекция перекрёстка
    # -------------------------------------------------------
    def isNearCrossRoad(self, img):
        pixels = cv2.countNonZero(
            cv2.inRange(
                img[320:480, 250:640],
                (155, 0, 0),
                (255, 100, 100)
            )
        )
        return pixels > 10000

    # -------------------------------------------------------
    # 3. Детекция дороги в направлении (лево / прямо / право)
    # -------------------------------------------------------
    def roadVisible(self, obs):
        """
        Детектирует дорогу в нижней части изображения.
        Не только жёлтую линию, но и просто серую дорогу.
        """
        roi = obs[300:460, :]
        mask = cv2.inRange(
            roi,
            (40, 40, 40),   # тёмно-серый
            (200, 200, 200) # светло-серый
        )
        area = cv2.countNonZero(mask)
        return area > 15000

    # -------------------------------------------------------
    # 4. Получение глобального угла направления на цель
    # -------------------------------------------------------
    def getGoalAngle(self, env, target_coords):
        pos = np.array([env.cur_pos[0], env.cur_pos[2]])
        goal = np.array([target_coords[0], target_coords[2]])
        
        # Учитываем отзеркаливание осей
        # Если Ox увеличивается влево, инвертируем x-компоненты
        vec_x = goal[0] - pos[0]
        vec_y = goal[1] - pos[1]
        
        # Инвертируем x-компоненту вектора
        # vec_x_inverted = -vec_x
        
        return math.atan2(-vec_y, vec_x)

    # -------------------------------------------------------
    # 5. Выбор лучшего поворота на перекрёстке
    # -------------------------------------------------------
    def chooseTurnDirection(self, env, obs, target_coords):
        """
        Возвращает одно из действий: 'left', 'forward', 'right'
        """

        # Угол к цели
        goal_angle = self.getGoalAngle(env, target_coords)

        candidates = []

        # ---- Проверка LEFT ----
        env.step([0, +1.0])   # слегка повернуть влево
        obs_l, _, _, _ = env.step([0, 0])
        env.step([0, -1.0])   # вернуть назад
        if self.roadVisible(obs_l):
            candidates.append(("left", env.cur_angle + math.radians(75)))

        # ---- Проверка FORWARD ----
        obs_f = obs
        if self.roadVisible(obs_f):
            candidates.append(("forward", env.cur_angle))

        # ---- Проверка RIGHT ----
        env.step([0, -1.0])   # слегка повернуть вправо
        obs_r, _, _, _ = env.step([0, 0])
        env.step([0, +1.0])   # вернуть назад
        if self.roadVisible(obs_r):
            candidates.append(("right", env.cur_angle - math.radians(75)))

        if not candidates:
            return "forward"   #fallback

        # Выбираем направление минимального разница углов
        def angle_diff(a, b):
            x = a - b
            return abs((x + math.pi) % (2*math.pi) - math.pi)

        best = min(
            candidates,
            key=lambda c: angle_diff(c[1], goal_angle)
        )
        return best[0]

    # -------------------------------------------------------
    # 6. Основной solve
    # -------------------------------------------------------
    def solve(self):
        
        
        def getRobotState(env):
            """
            Получение текущей позиции и угола робота.
            """
            pos = env.cur_pos          # (x, z, y) 
            angle = env.cur_angle      # угол в радианах
            return pos, angle
            
        def wrapAngle(a: float) -> float:
            """
            Нормализация угла в диапазон от -pi до pi

            Args:
                a (float): Угол в радианах

            Returns:
                float: Угол в диапазоне [-pi; pi]
            """
            return (a + np.pi) % (2 * np.pi) - np.pi
        
 
        
        
        def turnToAngle(env, target_angle, angular_speed=2.0, threshold=0.05):
            """
            Поворот робота до нужный угл.
            """
            while True:
                _, angle = getRobotState(env)
                err = wrapAngle(target_angle - angle)

                if abs(err) < threshold:    break

                # Управление вращением
                ang_vel = np.clip(err * 2.0, -angular_speed, angular_speed)
                env.step([0.0, ang_vel])
                env.render()     

        
        def driveStraight(env, distance_threshold=0.1, speed=0.1, target_pos=None):
            """
            Едет прямо до цели.
            """
            while True:
                pos, angle = getRobotState(env)
                dx = target_pos[0] - pos[0]
                dy = target_pos[2] - pos[2]

                dist = np.sqrt(dx*dx + dy*dy)
                if dist < distance_threshold:   break

                # Езда вперёд
                env.step([speed, 0.0])
                env.render()
        
        
        def driveToPoint(env, goal_pos):
            """
            1. Повернуть в сторону точки
            2. Ехать прямо
            """
            pos, angle = getRobotState(env)

            # Вычисляем угол к цели
            dx = goal_pos[0] - pos[0]
            dy = goal_pos[2] - pos[2]
            # goal_angle = np.arctan2(dy, dx)

            # Инвертируем dx для корректного расчета угла
            # dx_inverted = -dx
            goal_angle = np.arctan2(-dy, dx)
            # Повернуться
            turnToAngle(env, goal_angle)

            # Поехать
            driveStraight(env, target_pos=goal_pos)
        
        
        
        
        
        
        
        
        

        env = self.generated_task["env"]
        target_coordinates = self.generated_task["target_coordinates"][0]

        goal = np.array([target_coordinates[0], target_coordinates[2]])
        
        # self.initializeCar(env)

        step = 0
        print("Target:", goal)
        i = 0
        
        lane_safe_offset = 100
        while True:

            obs, _, _, _ = env.step([0, 0])
            
            # Проверка других роботов
            current_time = time.time()
            if current_time - self.last_robot_check > 0.5:
                robots = self.detectOtherRobots(obs)
                if robots:
                    self.avoidRobot(env, robots)
                    continue
                self.last_robot_check = current_time
            env.render()

            # Проверка достижения цели
            pos = np.array([env.cur_pos[0], env.cur_pos[2]])
            if np.linalg.norm(pos - goal) < 0.6:
                start_pos, start_angle = getRobotState(env)
                start = [start_pos[0], 0, start_pos[2]]
                
                # Target
                target_coordinates = self.generated_task['target_coordinates'][-1]
                goal = [target_coordinates[0], 0, target_coordinates[2]]
                
                print("start_pos ", start_pos)
                print("start_angle ", start_angle)
                for i in target_coordinates:
                    print("target_coordinates ", i)

                print("доехать до целевой точки")
                driveToPoint(env, goal)
                print(getRobotState(env))
                
                
                
                print("ПРИЕХАЛИ!")
                break

            # ---- Если перекрёсток ----
            if self.isNearCrossRoad(obs):

                turn = self.chooseTurnDirection(env, obs, target_coordinates)
                print("Crossroad! Choosing:", turn)

                # Выполняем выбранный манёвр
                if turn == "left":
                    action = [0.5, +1.0]
                elif turn == "right":
                    action = [0.5, -1.0]
                else:
                    action = [0.3, 0.0]

                # Выполняем небольшой манёвр
                for _ in range(35):
                    env.step(action)
                    env.render()
                    step += 1

                continue

            # ---- Обычное движение по линии ----
            # error, found = self.detectLaneOffset(obs)

            offset = self.detectRoadLine(obs)
            
            if offset == 0:
                print("Потеряcь полоса :(")
                obs, _, _, _ = env.step([0.0, -1.0])
                env.render()
                continue
            
            
            if offset > lane_safe_offset:
                angular = -0.6
                linear = 0.05
            elif offset < -lane_safe_offset:
                angular = 0.3
                linear = 0.1
            else:
                Kp = 0.005
                angular = -Kp * offset
                speed_factor = 1.0 - min(abs(offset) / 200.0, 0.6)
                linear = max(0.1, 0.5 * speed_factor)
                angular = max(-0.6, min(0.6, angular))
            
            action = [linear, angular]
            # if not found:
                # print("not found")
                # action = [0.0, -1.0]
                # while (not found) and i <= 8:
                # if i < 4: action = [0.0, -1.0]
                # elif i > 4 and i < 8: action = [0.0, 1.0]
                
                # if i >= 8: i = 0 
                # else: i += 1

                    # error, found = self.detectLaneOffset(obs)
                    # env.step(action)
                    # env.render()
                # action = [0.0, -float(self.getGoalAngle(env, target_coordinates))]
            # else:
                # i = 0
                # Kp = 0.005
                # angular = -Kp * error
                # linear = max(0.15, 0.9 - abs(angular))
                # action = [linear, angular]

            env.step(action)
            env.render()
            step += 1

        print("Mission complete!")
