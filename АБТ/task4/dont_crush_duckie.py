from gym_duckietown.tasks.task_solution import TaskSolution


class DontCrushDuckieTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)

    def solve(self):
        from cv2 import inRange, countNonZero
        from numpy import ascontiguousarray
        
        def duckNear(img):
            pixels = countNonZero(inRange(src=img, lowerb=(150, 150, 0), upperb=(255, 255, 150)))
            return pixels > 15000
        
        
        env = self.generated_task['env']
        obs, _, _, _ = env.step([0, 0])

        linear_velocity = 0.5
        angular_velocity = 0
        done_flag = False
        while (not done_flag):
            img = ascontiguousarray(obs)
            done_flag = duckNear(img)
            obs, _, _, _ = env.step([linear_velocity, angular_velocity])
            env.render()