#   Сканирование для сопоставления методом brute-force
import simulation_laser
import utils_poses_and_points

import numpy as np
from math import pi

from typing import List, Tuple
Point = Tuple[float, float]
Polygon = List[Point]
Edge = Tuple[Point, Point]


def scoreScanOnMap(grid: List, origin, resolution: float, points_world: List[Point]) -> List:
    nx, ny = grid.shape
    ix = ((points_world[:,0] - origin) / resolution).astype(int)
    iy = ((points_world[:,1] - origin) / resolution).astype(int)
    valid = (ix >= 0) & (ix < nx) & (iy >= 0) & (iy < ny)
    ix = ix[valid]
    iy = iy[valid]
    if len(ix) == 0:    return 0
    
    return grid[iy, ix].sum() / float(len(points_world))


def doScanMatchingMapBruteForce(grid: List, origin, resolution, pose1, ranges2, angles2,
                                search_radius=2.0,          # meters
                                coarse_steps=(21, 21, 36),  # nx,ny,ntheta
                                refine_iters=2):
    pts_local2, mask = simulation_laser.scanToPointsLocal(ranges2, angles2)
    # начальный центр: позиция 1
    center = pose1.copy()
    best_pose = None
    best_score = -1
    
    # многоуровневый перебор: сначала грубый, затем уточнения
    search_radius_level = search_radius
    for it in range(refine_iters):
        nx, ny, ntheta = coarse_steps
        dxs = np.linspace(-search_radius_level, search_radius_level, nx)
        dys = np.linspace(-search_radius_level, search_radius_level, ny)
        thetas = np.linspace(-pi, pi, ntheta, endpoint=False) if it==0 else np.linspace(-0.2, 0.2, ntheta)
        
        best_local_pose = None
        best_local_score = -1
        
        for dx in dxs:
            for dy in dys:
                # быстрый предварительный перевод
                trans = np.array([center[0] + dx, center[1] + dy])
                for dth in thetas:
                    cand_pose = np.array([trans[0], trans[1], utils_poses_and_points.wrapToPi(center[2] + dth)])
                    pts_world = utils_poses_and_points.transformPoints(pts_local2, cand_pose)
                    sc = scoreScanOnMap(grid, origin, resolution, pts_world)
                    if sc > best_local_score:
                        best_local_score = sc
                        best_local_pose = cand_pose
        # уточненный центр — best_local_pose
        center = best_local_pose
        best_score = best_local_score
        best_pose = best_local_pose
        # узкий радиус поиска для следующего iter
        search_radius_level = search_radius_level / 4.0
        
        coarse_steps = (11, 11, 41)
        
    pose_delta = utils_poses_and_points.poseMul(utils_poses_and_points.poseInv(pose1), best_pose)
    return pose_delta, best_pose, best_score