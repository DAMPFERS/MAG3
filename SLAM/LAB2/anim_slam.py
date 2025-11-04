import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation



from typing import List, Tuple
Point = Tuple[float, float]
Polygon = List[Point]
Edge = Tuple[Point, Point]

flag = True

def setupRealTimePlot():
    """Настройка интерактивного графика для отображения процесса SLAM"""
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
    fig.suptitle('SLAM в реальном времени', fontsize=14)
    
    # График 1: Комната и траектория
    ax1.set_title('Траектория и сканы')
    ax1.set_xlabel('X (м)')
    ax1.set_ylabel('Y (м)')
    ax1.grid(True)
    ax1.set_aspect('equal')
    
    # График 2: Карта занятости
    ax2.set_title('Occupancy Grid Map')
    ax2.set_xlabel('X (м)')
    ax2.set_ylabel('Y (м)')
    ax2.set_aspect('equal')
    
    return fig, ax1, ax2



def updateRealTimePlot(ax1, ax2, scan_data):
    """
    Обновление графика в реальном времени
    
    Args:
        ax1: ось для траектории и сканов
        ax2: ось для карты
        scan_data: словарь с данными для отображения
    """
    
    global flag
    # Очищаем предыдущие данные
    ax1.clear()
    ax2.clear()
    
    # Данные из словаря
    polygon = scan_data['polygon']
    poses = scan_data['poses']
    estimated_poses = scan_data['estimated_poses']
    current_scan_points = scan_data['current_scan_points']
    grid = scan_data['grid']
    origin = scan_data['origin']
    resolution = scan_data['resolution']
    current_scan_num = scan_data['current_scan_num']
    total_scans = scan_data['total_scans']
    
    # график 1: Комната и траектория
    ax1.set_title(f'Траектория и сканы ({current_scan_num}/{total_scans})')
    ax1.set_xlabel('X (м)')
    ax1.set_ylabel('Y (м)')
    ax1.grid(True)
    
    # отрисовка комнаты
    poly_plot = np.vstack((polygon, polygon[0]))
    ax1.plot(poly_plot[:, 0], poly_plot[:, 1], 'k-', linewidth=2, label='Комната')
    
    # траектории
    if len(poses) > 0:
        poses_array = np.array(poses[:current_scan_num])
        ax1.plot(poses_array[:, 0], poses_array[:, 1], 'go-', 
                markersize=6, linewidth=1, label='Истинная траектория', alpha=0.7)
    
    if len(estimated_poses) > 0:
        est_poses_array = np.array(estimated_poses)
        ax1.plot(est_poses_array[:, 0], est_poses_array[:, 1], 'ro-', 
                markersize=6, linewidth=1, label='Оцененная траектория', alpha=0.7)
        
        # Текущая поза
        current_pose = estimated_poses[-1]
        ax1.plot(current_pose[0], current_pose[1], 'rs', 
                markersize=10, label='Текущая поза')
    
    # Текущий скан
    if current_scan_points is not None and len(current_scan_points) > 0:
        ax1.scatter(current_scan_points[:, 0], current_scan_points[:, 1], 
                   c='blue', s=8, alpha=0.5, label='Текущий скан')
    
    ax1.legend()
    ax1.axis('equal')
    
    # график 2: Карта занятости
    ax2.set_title(f'Occupancy Grid Map ({current_scan_num}/{total_scans})')
    ax2.set_xlabel('X (м)')
    ax2.set_ylabel('Y (м)')
    
    if grid is not None:
        im = ax2.imshow(grid.T, origin='lower', 
                       extent=[origin[0], origin[0] + grid.shape[0] * resolution,
                               origin[1], origin[1] + grid.shape[1] * resolution],
                       cmap='RdYlBu_r', vmin=0, vmax=1, alpha=0.8)
        
        # цветовая шкала
        if flag:
            plt.colorbar(im, ax=ax2, label='Вероятность занятости')
            flag = False
        
        # траектория на карте
        if len(estimated_poses) > 0:
            est_poses_array = np.array(estimated_poses)
            ax2.plot(est_poses_array[:, 0], est_poses_array[:, 1], 'ro-', 
                    markersize=4, linewidth=1, label='Траектория')
            
            # текущая поза на карте
            current_pose = estimated_poses[-1]
            ax2.plot(current_pose[0], current_pose[1], 'rs', 
                    markersize=8, label='Текущая поза')
        
        ax2.legend()
        ax2.axis('equal')
    
    plt.tight_layout()
    plt.pause(0.1)  # пауза для обновления графика




def сalculateAndDisplayMetrics(polygon, grid, origin, resolution, poses, estimated_poses):
    """Вычисление и отображение метрик качества SLAM"""
    
    def calculatePolygonArea(poly: Polygon) -> float:
        """Вычисляет площадь полигона по формуле шнурования"""
        x = poly[:, 0]
        y = poly[:, 1]
        return 0.5 * np.abs(np.dot(x, np.roll(y, 1)) - np.dot(y, np.roll(x, 1)))
    
    # Вычисление площадей
    real_area = calculatePolygonArea(polygon)
    occupied_cells = np.sum(grid < 0.5)
    built_area = occupied_cells * (resolution ** 2) / 2
    area_error = abs(real_area - built_area) / real_area * 100
    
    # Вычисление ошибок позиционирования
    position_errors = []
    for i, (true_pose, est_pose) in enumerate(zip(poses, estimated_poses)):
        if i < len(estimated_poses):
            error = np.linalg.norm(true_pose[:2] - est_pose[:2])
            position_errors.append(error)
    
    avg_position_error = np.mean(position_errors) if position_errors else 0
    
    print(f"\n=== МЕТРИКИ КАЧЕСТВА SLAM ===")
    print(f"Реальная площадь комнаты: {real_area:.2f} м²")
    print(f"Построенная площадь: {built_area:.2f} м²")
    print(f"Ошибка площади: {area_error:.1f}%")
    print(f"Средняя ошибка позиционирования: {avg_position_error:.3f} м")
    print(f"Количество сканов: {len(poses)}")
    
    return real_area, built_area, area_error, avg_position_error





def saveSlamProgressGif(scan_history, filename='slam_progress.gif'):
    """
    Сохранение прогресса SLAM в виде GIF
    
    Args:
        scan_history: список словарей с историей сканов
        filename: имя файла для сохранения
    """
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
    
    def updateFrame(frame):
        ax1.clear()
        ax2.clear()
        
        data = scan_history[frame]
        
        # Левый график - траектория
        poly_plot = np.vstack((data['polygon'], data['polygon'][0]))
        ax1.plot(poly_plot[:, 0], poly_plot[:, 1], 'k-', linewidth=2)
        
        if len(data['estimated_poses']) > 0:
            est_poses = np.array(data['estimated_poses'])
            ax1.plot(est_poses[:, 0], est_poses[:, 1], 'ro-', markersize=3)
        
        ax1.set_title(f'Скан {frame+1}/{len(scan_history)}')
        ax1.set_aspect('equal')
        
        # Правый график - карта
        if data['grid'] is not None:
            ax2.imshow(data['grid'].T, origin='lower',
                      extent=[data['origin'][0], data['origin'][0] + data['grid'].shape[0] * data['resolution'],
                              data['origin'][1], data['origin'][1] + data['grid'].shape[1] * data['resolution']],
                      cmap='RdYlBu_r')
        
        ax2.set_aspect('equal')
    
    anim = FuncAnimation(fig, updateFrame, frames=len(scan_history), interval=500)
    anim.save(filename, writer='pillow', fps=2)
    print(f"Анимация сохранена как {filename}")