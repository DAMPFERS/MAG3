import pandas as pd
import numpy as np
from scipy import stats

from statsmodels.stats.diagnostic import lilliefors


import matplotlib.pyplot as plt
import seaborn as sns


# Загрузка данных из файла
data = pd.read_csv("./var4.csv", sep=';', encoding="cp1251")

print(data.columns)


# Удаление колонки "X.п.п", если она существует
if "№п/п" in data.columns:
    data = data.drop(columns=["№п/п"])
    

# Преобразование категориальных переменных в тип 'category'
data["группа"] = data["группа"].astype("category")
data["пол"] = data["пол"].astype("category")
data["качество документирования"] = data["качество документирования"].astype("category")
data["степень удовлетворенности заказчика (качественная оценка)"] = data["степень удовлетворенности заказчика (качественная оценка)"].astype("category")


# Создание подмножеств данных по группам
group1 = data[data["группа"] == 1]
group2 = data[data["группа"] == 2]

# Создание подмножеств данных по полу
gender1 = data[data["пол"] == 1]
gender2 = data[data["пол"] == 2]



import pandas as pd
import numpy as np
from scipy import stats
# from statsmodels.stats.diagnostic import lilliefors
# import matplotlib.pyplot as plt



# Выбираем количественный признак, например, "возраст"
feature = "возраст"

# Данные для первой и второй группы
group1_feature = group1[feature]
group2_feature = group2[feature]

# Функция для проверки нормальности распределения
def check_normality(data, group_name):
    print(f"\n--- Проверка нормальности для {group_name} ---")

    # Критерий Шапиро-Уилка
    shapiro_stat, shapiro_p = stats.shapiro(data)
    print(f"Шапиро-Уилк: статистика={shapiro_stat:.4f}, p-value={shapiro_p:.4f}")

    # Критерий Крамера-Мизеса (используем lilliefors как аналог)
    # Для Крамера-Мизеса нет встроенной функции в scipy, поэтому используем Lilliefors
    ks_stat, ks_p = stats.kstest(data, 'norm', args=(np.mean(data), np.std(data)))
    print(f"Колмогоров-Смирнов (аналог Крамера-Мизеса): статистика={ks_stat:.4f}, p-value={ks_p:.4f}")

    # Критерий Андерсона-Дарлинга
    anderson_result = stats.anderson(data, dist='norm')
    print(f"Андерсон-Дарлинг: статистика={anderson_result.statistic:.4f}")
    for i in range(len(anderson_result.critical_values)):
        sl, cv = anderson_result.significance_level[i], anderson_result.critical_values[i]
        if anderson_result.statistic < cv:
            print(f"  Уровень значимости {sl}%: данные распределены нормально (статистика < {cv})")
        else:
            print(f"  Уровень значимости {sl}%: данные НЕ распределены нормально (статистика >= {cv})")

# Проверяем нормальность для первой группы
check_normality(group1_feature, "группы 1")

# Проверяем нормальность для второй группы
check_normality(group2_feature, "группы 2")

# Критерий Шапиро-Уилка:
# Если p-value > 0.05, то данные распределены нормально.
# Если p-value ≤ 0.05, то данные не распределены нормально.

# Критерий Колмогорова-Смирнова (аналог Крамера-Мизеса):
# Если p-value > 0.05, то данные распределены нормально.
# Если p-value ≤ 0.05, то данные не распределены нормально.

# Критерий Андерсона-Дарлинга:
# Сравниваем статистику с критическими значениями для разных уровней значимости.
# Если статистика меньше критического значения, данные распределены нормально.
