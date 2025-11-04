from gym_duckietown.tasks.task_solution import TaskSolution


class DefaultTaskSolution(TaskSolution):
    def __init__(self, generated_task):
        super().__init__(generated_task)

    def solve(self):
        env = self.generated_task["env"]
        # target_coordinates = self.generated_task['target_coordinates'][-1]
        linear_velocity = 0
        angular_velocity = 0
        while True:
            obs, _, _, _ = env.step([linear_velocity, angular_velocity])
            env.render()


if __name__ == "__main__":
    # код ниже требуется для возможности запуска вашего решения в описываемом образе, при отправки решения в систему проверки данный код не требуется
    from gym_duckietown.tasks.default.task_generator import DefaultTaskGenerator

    task_generator = DefaultTaskGenerator()
    task_generator.generate_task()
    solution = DefaultTaskSolution(task_generator.generated_task)
    solution.solve()


