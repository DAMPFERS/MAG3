from gym_duckietown.tasks.task_solution import TaskSolution
from gym_duckietown.objects import TrafficLightObj



class TrafficlightTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)

    def solve(self):
        
        from time import time
        
        def stepFunc(env, velocity=0):
            env.step([velocity, 0])
            env.render()

        def moveFunc(env, velocity, duration):
            t = time()
            while True:
                dt =  time() - t
                env.step([velocity, 0])
                if dt >= duration:
                    return
        
        
        
        env = self.generated_task['env']
        trafficLights = []
        i = 0
        for obj in env.objects:
            if isinstance(obj, TrafficLightObj):
                trafficlight = obj
                trafficLights.append(trafficlight)
        moveFunc(env, 0.4, 2)
        while True:
            if trafficLights[i].pattern == 1:
                i += 1
                break
            stepFunc(env)


        for trafficLight in range(6):
            moveFunc(env, 0.4, 3)
            if i < len(trafficLights):
                while True:
                    if trafficLights[i].pattern == 1:
                        i += 1
                        break
                    stepFunc(env)                
        
        stepFunc(env)
        print("FINISHED")