from gym_duckietown.tasks.task_solution import TaskSolution
import numpy as np
from numpy import ascontiguousarray
from cv2 import inRange, countNonZero
import math

class LfChallengeNoCvTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)

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
        
        def distance(p1, p2):
            return np.linalg.norm(np.array(p1[:2]) - np.array(p2[:2]))  
        
        
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
            goal_angle = np.arctan2(dy, dx)

            # Повернуться
            turnToAngle(env, goal_angle)

            # Поехать
            driveStraight(env, target_pos=goal_pos)
        
        
        
        env = self.generated_task['env']
        obs, _, _, _ = env.step([0, 0])  
        env.render()

        # Получаем стартовую позицию и целевые координаты
        # start_pos = env.cur_pos.copy()
        # start_pos = env.unwrapped.cur_pos
        
        #START
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

        print("развернуться на 180")
        _, current_angle = getRobotState(env)
        target_angle = wrapAngle(current_angle + np.pi)
        turnToAngle(env, target_angle)
        
        print("вернуться в исходную точку")
        driveToPoint(env, start)
        
        # print("финальное выравнивание по исходному углу")
        # turnToAngle(env, start_angle)

        print("YES")

        # Останавливаем робота
        env.step([0, 0])
        env.render()
    
    
    
    