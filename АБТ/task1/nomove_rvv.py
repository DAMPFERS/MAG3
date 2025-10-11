from gym_duckietown.tasks.task_solution import TaskSolution


class NoMoveTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)
  
    def solve(self):
        env = self.generated_task['env']
        # target_coordinates = self.generated_task['target_coordinates'][-1]
        linear_velocity = 0
        angular_velocity = 0
        while True:
            obs, _, _, _ = env.step([linear_velocity, angular_velocity])
            env.render()




# if __name__ == "__main__":
    # from gym_duckietown.envs import DuckietownEnv

    # env = DuckietownEnv(
    #     map_name='straight_road',
    #     draw_curve=False,
    #     domain_rand=False,
    #     #camera_rand=False,
    #     user_tile_start=(1, 0),
    #     render_mode='none'
    #     #start_pose=[0.5, 0, 0.5, 0]  # x, y, z, θ
    # )

    # generated_task = {
    #     'env': env,
    #     'target_coordinates': [(0.5, 0.5)]
    # }

    # solution = NoMoveTaskSolution(generated_task)
    # solution.solve()
    # env.close()

    # print("0_o")

