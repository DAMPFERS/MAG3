import numpy as np
def wrap(a):
    return (a + np.pi) % (2*np.pi) - np.pi


def test_angles():
    pos = [100.0, 0, 0.0]
    
    test_cases = [
        ([100.0, 0, 0.0], 0.0, "прямо вперед"),
        ([-100.0, 0, 0.0], np.pi, "прямо назад"), 
        ([0.0, 0, 100.0], np.pi/2, "направо"),
        ([0.0, 0, -100.0], -np.pi/2, "налево")
    ]
    
    # print("Способ 1 - инвертировать  компоненту x:")
    for goal_pos, expected, desc in test_cases:
        dx = (goal_pos[0] - pos[0])
        dy = goal_pos[2] - pos[2]

        goal_angle = np.arctan2(-dy, dx)
        goal_angle = wrap(goal_angle)
        print(f"{desc}: должно быть {expected:.2f}, имеем: {goal_angle:.2f}")
    
    

test_angles()