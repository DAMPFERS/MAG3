from gym_duckietown.tasks.task_solution import TaskSolution




class Ride1MTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)

    def solve(self):
        from time import time
        
        env = self.generated_task['env']
        distance = 1
        linear_velocity = 1 
        angular_velocity = 0  
        
        start_time = time()      
        while True:
            obs, _, _, _ = env.step([linear_velocity, angular_velocity])
            current_time = time()
            if (current_time - start_time) * linear_velocity >= distance:   break
            env.render()

        env.render()    
        