from gym_duckietown.tasks.task_solution import TaskSolution
import cv2
import math
import time
import numpy as np


class RandomMapAggressiveTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)
        self.tile_size = 0.585
        self.right_lane_time = 0
        self.total_time = 0
        self.last_robot_check = 0

    def dist_to_target(self, my_pos, target_pos):
        dx = target_pos[0] - my_pos[0]
        dz = target_pos[2] - my_pos[2]
        return math.sqrt(dx**2 + dz**2)

    def angle_normalize(self, angle):
        return (angle + math.pi) % (2 * math.pi) - math.pi

    def turn_to_target(self, my_pos, target_pos):
        dx = target_pos[0] - my_pos[0]
        dz = target_pos[2] - my_pos[2]
        return math.atan2(-dz, dx)

    def get_count_pixels(self, image, color, is_print=False) -> int:
        color_start_range = (0,0,0)
        color_end_range = (0,0,0)

        if color == "yellow":
            color_start_range=(150,150,0)
            color_end_range=(255,255,150)
        elif color == "red":
            color_start_range=(155, 0, 0)
            color_end_range=(255, 100, 100)
        elif color == "white":
            color_start_range=(180, 180, 180)
            color_end_range=(255, 255, 255)
        elif color == "road":
            color_start_range=(50, 50, 50)
            color_end_range=(150, 150, 150)

        color_mask = cv2.inRange(image, color_start_range, color_end_range)
        pixels_count = cv2.countNonZero(color_mask)
        if is_print:
            print(f"{color}: {pixels_count} пикселей")
        return pixels_count

    def detect_stop_line(self, obs):
        obs2 = obs[250:480, 0:640]
        color_start_range = (155, 0, 0)
        color_end_range = (255, 100, 100)
        color_filter_mask = cv2.inRange(
            src=obs2,
            lowerb=color_start_range,
            upperb=color_end_range
        )
        detected_pixels = cv2.countNonZero(color_filter_mask)
        return detected_pixels > 10000

    def detect_road_line(self, obs):
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

    def detect_other_robots(self, obs):
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

    def initialize_car(self, env):
        for i in range(80):
            line = env.get_lane_pos2(env.cur_pos, env.cur_angle)
            
            # Простой П-регулятор
            angular_velocity = line.angle_rad * 3.0 - line.dist * 4.0
            
            if abs(angular_velocity) < 0.4:  # Минимум 0.4!
                angular_velocity = 0.4 if angular_velocity > 0 else -0.4
            
            angular_velocity = max(-1.0, min(1.0, angular_velocity))
            
            # Всегда двигаемся вперед
            env.step([0.06, angular_velocity])
            env.render()
            
            if i % 20 == 0:
                print(f"Шаг {i}: steering={angular_velocity:.2f}")
            
            if abs(line.dist) < 0.03 and abs(line.angle_rad) < math.radians(10):
                print(f"Выровнено за {i} шагов")
                return True
        #
        print("Выравнивание было")
        return True

    def turning_to_target(self, env, target_pos, speed=1.0):
        target_angle = self.turn_to_target(env.cur_pos, target_pos)
        while True:
            angle = self.angle_normalize(target_angle - env.cur_angle)
            if abs(angle) < 0.1:
                break
            steer_speed = np.clip(angle * 2.0, -speed, speed)
            env.step([0.0, steer_speed])
            env.render()

    def determine_turn_direction(self, env, target_pos):
        angle = self.turn_to_target(env.cur_pos, target_pos)
        angle_diff = self.angle_normalize(angle - env.cur_angle)
        angle_diff_degrees = math.degrees(angle_diff)

        priority = []
        if abs(angle_diff_degrees) < 45:
            priority.append('straight')
        
        if angle_diff > 0:
            priority.append('left')
            if len(priority) == 1:
                priority.append('straight')
            priority.append('right')
        else:
            priority.append('right')
            if len(priority) == 1:
                priority.append('straight')
            priority.append('left')
        return priority

    def drive_straight(self, env, speed):
        frames = self.tile_size / (speed / 30)
        for _ in range(int(frames)):
            obs, _, _, _ = env.step([speed, 0.0])
            env.render()
            if self.detect_road_line(obs) > 300:
                return

    def drive_left(self, env, speed):
        frames = (self.tile_size / 2) / (speed / 30)
        for _ in range(int(frames + 10)):
            obs, _, _, _ = env.step([speed, 0.0])
            env.render()
            if self.detect_road_line(obs) > 300:
                return
        target_angle = env.cur_angle + math.pi / 2
        target_angle = self.angle_normalize(target_angle)
        while abs(self.angle_normalize(env.cur_angle) - target_angle) > 0.1:
            obs, _, _, _ = env.step([0.0, 0.5])
            env.render()
        for _ in range(int(frames)):
            obs, _, _, _ = env.step([speed, 0.0])
            env.render()

    def drive_right(self, env, speed):
        frames = (self.tile_size / 2 * 1.2) / (speed / 30)
        for _ in range(int(frames + 10)):
            obs, _, _, _ = env.step([speed, 0.0])
            env.render()
            if self.detect_road_line(obs) > 300:
                return
        target_angle = env.cur_angle - math.pi / 2
        target_angle = self.angle_normalize(target_angle)
        while abs(self.angle_normalize(env.cur_angle) - target_angle) > 0.1:
            obs, _, _, _ = env.step([0.0, -0.5])
            env.render()
        for _ in range(int(frames)):
            obs, _, _, _ = env.step([speed, 0.0])
            env.render()

    def avoid_robot(self, env, robots):
        # Объезд робота с левой стороны
        print("Объезд робота")
        for _ in range(15):
            env.step([0.2, 0.6])
            env.render()
        for _ in range(20):
            env.step([0.3, 0.0])
            env.render()
        for _ in range(20):
            env.step([0.2, -0.6])
            env.render()
        print("Объезд завершен")

    def final_approach(self, env, target_pos):
        print("Финальное приближение")
        print(f"Моя позиция x={env.cur_pos[0]:.3f}, z={env.cur_pos[2]:.3f}")
        print(f"Целевая позиция: x={target_pos[0]:.3f}, z={target_pos[2]:.3f}")
        print(f"Расстояние {self.dist_to_target(env.cur_pos, target_pos)}")

        self.turning_to_target(env, target_pos, 0.3)
        speed = 0.1
        while True:
            self.turning_to_target(env, target_pos)
            dx = target_pos[0] - env.cur_pos[0]
            dy = target_pos[2] - env.cur_pos[2]
            dist = np.sqrt(dx*dx + dy*dy)
            if dist < 0.05:
                break
            env.step([speed, 0.0])
            env.render()

        final_dist = self.dist_to_target(env.cur_pos, target_pos)
        print(f"Цель достигнута! Финальное расстояние: {final_dist:.3f} м")

    def solve(self):
        env = self.generated_task['env']
        target_pos = self.generated_task['target_coordinates'][0]

        print(f"Старт: x={env.cur_pos[0]:.3f}, z={env.cur_pos[2]:.3f}")
        print(f"Цель: x={target_pos[0]:.3f}, z={target_pos[2]:.3f}")

        self.initialize_car(env)

        kp = 0.002
        lane_safe_offset = 100

        while self.dist_to_target(env.cur_pos, target_pos) > self.tile_size:
            obs, _, _, _ = env.step([0, 0])

            # Проверка других роботов
            current_time = time.time()
            if current_time - self.last_robot_check > 0.5:
                robots = self.detect_other_robots(obs)
                if robots:
                    self.avoid_robot(env, robots)
                    continue
                self.last_robot_check = current_time

            # Перекресток
            if self.detect_stop_line(obs):
                print("Перекресток")
                turn_direction = self.determine_turn_direction(env, target_pos)
                print(f"Направление: {turn_direction[0]}")

                if turn_direction[0] == 'straight':
                    self.drive_straight(env, 0.25)
                elif turn_direction[0] == 'left':
                    self.drive_left(env, 0.25)
                elif turn_direction[0] == 'right':
                    self.drive_right(env, 0.25)

                print("Перекресток пройден")
                continue

            # Основное движение по правой полосе
            offset = self.detect_road_line(obs)
            
            # Обновляем статистику правой полосы
            self.total_time += 1
            if offset < 100:  # Желтая полоса слева = мы в правой полосе
                self.right_lane_time += 1

            if offset == 0:
                print("Потеря полосы")
                obs, _, _, _ = env.step([0.1, 0.5])
                env.render()
                continue

            # Контроль движения
            if offset > lane_safe_offset:
                angular = -0.6
                linear = 0.05
            elif offset < -lane_safe_offset:
                angular = 0.3
                linear = 0.1
            else:
                angular = -kp * offset
                speed_factor = 1.0 - min(abs(offset) / 200.0, 0.6)
                linear = max(0.1, 0.5 * speed_factor)
                angular = max(-0.6, min(0.6, angular))

            obs, _, _, _ = env.step([linear, angular])
            env.render()

        self.final_approach(env, target_pos)

        # Финальная статистика
        if self.total_time > 0:
            right_percent = (self.right_lane_time / self.total_time) * 100
            print(f"Правой полосы: {right_percent:.1f}%")
            if right_percent >= 80:
                print("Требование выполнено")
            else:
                print("Требование не выполнено")