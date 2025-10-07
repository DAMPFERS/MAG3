import pandas as pd
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt
import seaborn as sns



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
# 1 Диаграмма рассеяния по двум количественным признакам
# Диаграмма рассеяния: Возраст vs Стаж работы
plt.figure(figsize=(10, 6)) # задается полотно размером 10 на 6
sns.scatterplot(        #   инициализация диаграммы
    data=data,          #   источник данных
    x="возраст",        #   параметр признак
    y="стаж работы",    #   параметр признак
    hue="группа",       #   параметр разделения точек (по цвету)
    palette="viridis",  #   Настройка палитры
    s=50,               #   Размер точек графика
    alpha=0.7           #   прозрачность точек графика
)
plt.title("Диаграмма рассеяния: Возраст vs Стаж работы") # Название диаграммы
plt.xlabel("Возраст")   # Подпись оси абсцис
plt.ylabel("Стаж работы")   # Подпись оси ординат
plt.grid(True)  #   Включение сетки
plt.show()  #   Отображение графики




# 2 Радиальная диаграмма по качественному признаку

# Радиальная диаграмма для "степень удовлетворенности заказчика (качественная оценка)"
plt.figure(figsize=(8, 8))  # задается полотно размером 8 на 8
counts = data["степень удовлетворенности заказчика (качественная оценка)"].value_counts() # Извлекает данные из столбца, получается Series, где индексы - это категории удовлетворенности, а значения - их частоты
print(counts)
plt.pie(    # Строит круговую диаграмму
    counts, # данные для построения (количества по категориям)
    labels=counts.index,    # подписи для секторов (названия категорий)
    autopct="%1.1f%%",      # отображает проценты на диаграмме с одним знаком после запятой
    startangle=90,          # начинает построение с 90 градусов (сверху)
    colors=sns.color_palette("Blues", len(counts))  # задает цветовую палитру "Blues"
)
plt.title("Степень удовлетворенности заказчика (радиальная диаграмма)") # Название диаграммы
plt.show()  #   Отображение графики


# 3. Категориальная радиальная диаграмма по качественному признаку в зависимости от пола и группы
# Категориальная радиальная диаграмма по "качество.документирования" в зависимости от пола и группы
fig, axes = plt.subplots(1, 2, figsize=(14, 6), subplot_kw=dict(polar=True))   # Создает полотно с двумя подграфиками рядом друг с другом (1 строка, 2 столбца)
# ubplot_kw=dict(polar=True) - ключевой параметр: создает радиальные (полярные) координаты вместо декартовых
# fig - общее полотно, axes - массив из двух осей для радиальных графиков

# Для группы 1 и группы 2
for i, group in enumerate([group1, group2], start=1):
    counts = group["качество документирования"].value_counts()  # Подсчитывает частоты уникальных значений в столбце "качество документирования" для текущей группы
    axes[i-1].bar(                  # Строит радиальные столбцы на соответствующем подграфике
        x=range(len(counts)),       # позиции столбцов по кругу (в радианах)
        height=counts,              # высота столбцов (значения частот)
        width=0.6,                  #  ширина столбцов в радианах
        bottom=0.0,                 #  основание столбцов (в центре диаграммы)
        color=sns.color_palette("viridis", len(counts)),    # цветовая палитра "viridis"
        alpha=0.7                   # прозрачность
    )
    axes[i-1].set_title(f"Группа {i}: Качество документирования")   # Устанавливает заголовок для каждого подграфика с номером группы
#     # Настраивает метки на оси: позиции и подписи (категории качества документирования)
    axes[i-1].set_xticks(range(len(counts)))
    axes[i-1].set_xticklabels(counts.index)
    
    axes[i-1].grid(True)    # Добавляет сетку на радиальную диаграмму
    

plt.tight_layout()  # Автоматически регулирует отступы между подграфиками
plt.show()  #   Отображение графика



# 4. Категориальная столбиковая диаграмма по количественному признаку в зависимости от пола и группы
# Столбиковая диаграмма: Средняя степень удовлетворенности заказчика (балльная оценка)
# Создаем DataFrame для средних значений степени удовлетворенности заказчика
quality_data = pd.DataFrame([
    {
        "Категория": "Группа 1",
        "Средняя степень": group1["степень удовлетворенности заказчика (балльная оценка)"].mean()
    },
    {
        "Категория": "Группа 2",
        "Средняя степень": group2["степень удовлетворенности заказчика (балльная оценка)"].mean()
    },
    {
        "Категория": "Пол 1",
        "Средняя степень": gender1["степень удовлетворенности заказчика (балльная оценка)"].mean()
    },
    {
        "Категория": "Пол 2",
        "Средняя степень": gender2["степень удовлетворенности заказчика (балльная оценка)"].mean()
    }
])

# # Столбиковая диаграмма: Средняя степень удовлетворенности заказчика (балльная оценка)
plt.figure(figsize=(10, 6))
sns.barplot(
    data=quality_data,  # Данные
    x="Категория",      # Подпись
    y="Средняя степень",# Подпись
    palette="viridis"   # Настройка палитры
)
plt.title("Средняя степень удовлетворенности заказчика (балльная оценка)")
plt.xlabel("")
plt.ylabel("Средняя степень удовлетворенности")
plt.grid(True)
plt.show()



# 5. Диаграмма размаха для количественного признака в зависимости от группы
# # Диаграмма размаха: % выполнения разработок в срок
plt.figure(figsize=(10, 6))
sns.boxplot(
    data=data,
    x="группа",
    y="% выполнения разработок в срок, в рамках бюджета, с требуемым функционалом",
    palette="viridis"
)
plt.title("Диаграмма размаха: % выполнения разработок в срок")
plt.xlabel("Группа")
plt.ylabel("% выполнения разработок в срок")
plt.grid(True)
plt.show()

# 6. Гистограммы для всех количественных признаков
# Гистограммы для всех количественных признаков

# фильтрует только столбцы с целочисленными и вещественными типами данных
numeric_columns = data.select_dtypes(include=["int64", "float64"]).columns  # содержит список имен всех числовых столбцов

fig, axes = plt.subplots(3, 2, figsize=(14, 12))    # Создает полотно с 6 подграфиками в виде сетки 3×2
axes = axes.flatten()                               # Выравнивает массив осей из формата 3×2 в одномерный массив из 6 элементов

for i, column in enumerate(numeric_columns):        # Организует цикл по всем числовым столбцам
    sns.histplot(data[column], ax=axes[i], kde=True, color="skyblue")   # Строит гистограмму для текущего числового столбца
#     # data[column] - данные для построения
#     # ax=axes[i] - указывает, на каком подграфике строить
#     # kde=True - добавляет Kernel Density Estimation (гладкую линию оценки плотности)
#     # color="skyblue" - задает голубой цвет гистограммы
    
    axes[i].set_title(f"Гистограмма: {column}") # Устанавливает заголовок для каждого подграфика с названием переменной
    axes[i].set_xlabel("")                      # Убирает подпись оси X (оставляет пустую строку) для более компактного вида
    axes[i].grid(True)                          # Добавляет сетку на график

plt.tight_layout()
plt.show()



# 7. Матричный график (опционально)
# Матричный график (pairs plot)

numeric_columns = data.select_dtypes(include=["int64", "float64"]).columns  # содержит список имен всех числовых столбцов
# Создает матричный график (pairplot) - сетку графиков "каждый с каждым"
# data=data[numeric_columns] - использует только числовые столбцы из DataFrame
# diag_kind="kde" - на диагональных графиках вместо гистограмм использует Kernel Density Estimation (гладкие кривые распределения)
# plot_kws={"alpha": 0.6, "s": 50, "edgecolor": "k"} - параметры для точечных графиков:
# alpha=0.6 - прозрачность 60% (позволяет видеть перекрывающиеся точки)
# s=50 - размер точек (size)
# edgecolor="k" - черная обводка вокруг точек (улучшает видимость)
# height=2.5 - высота каждого отдельного подграфика в дюймах

sns.pairplot(
    data=data[numeric_columns],
    diag_kind="kde",
    plot_kws={"alpha": 0.6, "s": 50, "edgecolor": "k"},
    height=2.5
)
plt.suptitle("Матричный график", y=1.02)
# Добавляет общий заголовок для всей матрицы графиков
# y=1.02 - немного поднимает заголовок выше (102% от обычной позиции) для лучшего визуального размещения
plt.show()



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




# 6.1. Корреляция между качественными переменными (Chi-квадрат и Фишер)
# Модули
import pandas as pd
import scipy.stats as stats
from scipy.stats import chi2_contingency, fisher_exact

# Качественные переменные: "пол" и "степень удовлетворенности заказчика (качественная оценка)"

# Для группы 1
contingency_table_group1 = pd.crosstab(
    group1["пол"],
    group1["степень удовлетворенности заказчика (качественная оценка)"]
)

# # Критерий χ² (Chi-квадрат)
chi2_group1, p_chi2_group1, _, _ = chi2_contingency(contingency_table_group1)

# # Критерий Фишера (только если таблица 2x2)
if contingency_table_group1.shape == (2, 2):
    _, p_fisher_group1 = fisher_exact(contingency_table_group1)
else:
    p_fisher_group1 = None
    print("Таблица не 2x2, критерий Фишера не применим.")

# # Для группы 2
contingency_table_group2 = pd.crosstab(
    group2["пол"],
    group2["степень удовлетворенности заказчика (качественная оценка)"]
)

# # Критерий χ² (Chi-квадрат)
chi2_group2, p_chi2_group2, _, _ = chi2_contingency(contingency_table_group2)

# # Критерий Фишера (только если таблица 2x2)
if contingency_table_group2.shape == (2, 2):
    _, p_fisher_group2 = fisher_exact(contingency_table_group2)
else:
    p_fisher_group2 = None
    print("Таблица не 2x2, критерий Фишера не применим.")

# # Вывод результатов
print("Группа 1:")
print(f"Chi-квадрат: p-value = {p_chi2_group1:.4f}")
if p_fisher_group1 is not None:
    print(f"Фишер: p-value = {p_fisher_group1:.4f}")
else:
    print("Критерий Фишера не применим.")

print("\nГруппа 2:")
print(f"Chi-квадрат: p-value = {p_chi2_group2:.4f}")
if p_fisher_group2 is not None:
    print(f"Фишер: p-value = {p_fisher_group2:.4f}")
else:
    print("Критерий Фишера не применим.")





# 6.2. Однофакторный дисперсионный анализ (ANOVA) и критерий Краскела-Уоллиса
from scipy.stats import f_oneway, kruskal

# # Выбираем количественную переменную: "% выполнения разработок в срок"
# # и качественную переменную: "степень.удовлетворенности.заказчика..качественная.оценка."

# # ANOVA
anova_groups = [
    group["% выполнения разработок в срок, в рамках бюджета, с требуемым функционалом"]
    for _, group in data.groupby("степень удовлетворенности заказчика (качественная оценка)")
]
f_stat, p_anova = f_oneway(*anova_groups)

# # Краскела-Уоллиса
kruskal_stat, p_kruskal = kruskal(*anova_groups)

print(f"ANOVA: F-статистика = {f_stat:.4f}, p-value = {p_anova:.4f}")
print(f"Краскела-Уоллиса: статистика = {kruskal_stat:.4f}, p-value = {p_kruskal:.4f}")



# 6.3. Коэффициенты корреляции Пирсона, Спирмена, Кендалла
# Выбираем количественные переменные
numeric_data = data.select_dtypes(include=["int64", "float64"])

# # Для группы 1
corr_pearson_group1 = group1[numeric_data.columns].corr(method="pearson")
corr_spearman_group1 = group1[numeric_data.columns].corr(method="spearman")
corr_kendall_group1 = group1[numeric_data.columns].corr(method="kendall")

# # Для группы 2
corr_pearson_group2 = group2[numeric_data.columns].corr(method="pearson")
corr_spearman_group2 = group2[numeric_data.columns].corr(method="spearman")
corr_kendall_group2 = group2[numeric_data.columns].corr(method="kendall")

print("Коэффициенты корреляции для группы 1:")
print("Пирсон:\n", corr_pearson_group1)
print("Спирмен:\n", corr_spearman_group1)
print("Кендалл:\n", corr_kendall_group1)

print("\nКоэффициенты корреляции для группы 2:")
print("Пирсон:\n", corr_pearson_group2)
print("Спирмен:\n", corr_spearman_group2)
print("Кендалл:\n", corr_kendall_group2)



# 6.4. Частный коэффициент корреляции
from pingouin import partial_corr

# # Выбираем две количественные переменные с максимальным коэффициентом корреляции Пирсона
# # Например, "возраст" и "% выполнения разработок в срок"
var1 = "возраст"
var2 = "% выполнения разработок в срок, в рамках бюджета, с требуемым функционалом"

# # Для группы 1
partial_corr_group1 = partial_corr(
    data=group1,
    x=var1,
    y=var2,
    covar=["стаж работы"]
)

# # Для группы 2
partial_corr_group2 = partial_corr(
    data=group2,
    x=var1,
    y=var2,
    covar=["стаж работы"]
)

print("Частный коэффициент корреляции для группы 1:")
print(partial_corr_group1)

print("\nЧастный коэффициент корреляции для группы 2:")
print(partial_corr_group2)




# 6.5. Тепловая карта матрицы корреляции и матричный график


import seaborn as sns
import matplotlib.pyplot as plt


numeric_data = data.select_dtypes(include=["int64", "float64"])
corr_pearson_group1 = group1[numeric_data.columns].corr(method="pearson")
corr_pearson_group2 = group2[numeric_data.columns].corr(method="pearson")

# Тепловая карта для группы 1
plt.figure(figsize=(10, 8))
sns.heatmap(corr_pearson_group1, annot=True, cmap="coolwarm", center=0)
plt.title("Матрица корреляции Пирсона для группы 1")
plt.show()

# Тепловая карта для группы 2
plt.figure(figsize=(10, 8))
sns.heatmap(corr_pearson_group2, annot=True, cmap="coolwarm", center=0)
plt.title("Матрица корреляции Пирсона для группы 2")
plt.show()

# Матричный график (аналог ggpairs)
sns.pairplot(group1[numeric_data.columns])
plt.suptitle("Матричный график для группы 1", y=1.02)
plt.show()

sns.pairplot(group2[numeric_data.columns])
plt.suptitle("Матричный график для группы 2", y=1.02)
plt.show()