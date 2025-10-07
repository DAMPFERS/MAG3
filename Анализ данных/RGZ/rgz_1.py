import pandas as pd
import numpy as np
from scipy import stats

# Загрузка данных из файла
data = pd.read_csv("./var4.csv", sep=';', encoding="cp1251")

print(data.columns)


# Удаление колонки "№п/п", если она существует
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






# Функция для поиска моды





# def findModeDF(df):
    
    
#     print("Мода для каждой колонки:")
#     for column in df.columns:
#         if df[column].dtype in ["int64", "float64", "category"]:
#             mode_value = find_mode(df[column])
#             print(f"{column}: {mode_value}")
            
# Расчет основных статистических характеристик



def calculateStatistics(df, group_name):
    
    def find_mode(series):
        # Находит моду (наиболее часто встречающееся значение) в серии
        mode_values = series.mode()
        return mode_values.tolist()
    
    print(f"\nСтатистические характеристики для {group_name}:")

    # Выбираем только количественные столбцы
    numeric_columns = df.select_dtypes(include=["int64", "float64"]).columns

    for column in numeric_columns:
        column_data = df[column]
        print(f"\nСтолбец: {column}")
        print(f"Минимальное значение: {column_data.min()}")
        print(f"Максимальное значение: {column_data.max()}")
        print(f"Среднее значение: {column_data.mean()}")
        print(f"Стандартное отклонение: {column_data.std()}")
        print(f"Первый квартиль: {column_data.quantile(0.25)}")
        print(f"Третий квартиль: {column_data.quantile(0.75)}")
        print(f"Медиана: {column_data.median()}")
        print(f"Мода: {find_mode(column_data)[0]}")
        print(f"Асимметрия: {stats.skew(column_data)}")
        print(f"Эксцесс: {stats.kurtosis(column_data)}")


# Рассчитываем статистику для всей выборки
calculateStatistics(data, "всей выборки")

# Рассчитываем статистику для первой группы
calculateStatistics(group1, "группы 1")

# Рассчитываем статистику для второй группы
calculateStatistics(group2, "группы 2")





######################