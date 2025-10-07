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




# 6.1. Корреляция между качественными переменными (Chi-квадрат и Фишер)
# Модули
import pandas as pd
import scipy.stats as stats
from scipy.stats import chi2_contingency, fisher_exact

# Качественные переменные: "пол" и "степень удовлетворенности заказчика (качественная оценка)"

# Для группы 1
# contingency_table_group1 = pd.crosstab(
#     group1["пол"],
#     group1["степень удовлетворенности заказчика (качественная оценка)"]
# )

# # Критерий χ² (Chi-квадрат)
# chi2_group1, p_chi2_group1, _, _ = chi2_contingency(contingency_table_group1)

# # Критерий Фишера (только если таблица 2x2)
# if contingency_table_group1.shape == (2, 2):
#     _, p_fisher_group1 = fisher_exact(contingency_table_group1)
# else:
#     p_fisher_group1 = None
#     print("Таблица не 2x2, критерий Фишера не применим.")

# # Для группы 2
# contingency_table_group2 = pd.crosstab(
#     group2["пол"],
#     group2["степень удовлетворенности заказчика (качественная оценка)"]
# )

# # Критерий χ² (Chi-квадрат)
# chi2_group2, p_chi2_group2, _, _ = chi2_contingency(contingency_table_group2)

# # Критерий Фишера (только если таблица 2x2)
# if contingency_table_group2.shape == (2, 2):
#     _, p_fisher_group2 = fisher_exact(contingency_table_group2)
# else:
#     p_fisher_group2 = None
#     print("Таблица не 2x2, критерий Фишера не применим.")

# # Вывод результатов
# print("Группа 1:")
# print(f"Chi-квадрат: p-value = {p_chi2_group1:.4f}")
# if p_fisher_group1 is not None:
#     print(f"Фишер: p-value = {p_fisher_group1:.4f}")
# else:
#     print("Критерий Фишера не применим.")

# print("\nГруппа 2:")
# print(f"Chi-квадрат: p-value = {p_chi2_group2:.4f}")
# if p_fisher_group2 is not None:
#     print(f"Фишер: p-value = {p_fisher_group2:.4f}")
# else:
#     print("Критерий Фишера не применим.")





# 6.2. Однофакторный дисперсионный анализ (ANOVA) и критерий Краскела-Уоллиса
# from scipy.stats import f_oneway, kruskal

# # Выбираем количественную переменную: "% выполнения разработок в срок"
# # и качественную переменную: "степень.удовлетворенности.заказчика..качественная.оценка."

# # ANOVA
# anova_groups = [
#     group["% выполнения разработок в срок, в рамках бюджета, с требуемым функционалом"]
#     for _, group in data.groupby("степень удовлетворенности заказчика (качественная оценка)")
# ]
# f_stat, p_anova = f_oneway(*anova_groups)

# # Краскела-Уоллиса
# kruskal_stat, p_kruskal = kruskal(*anova_groups)

# print(f"ANOVA: F-статистика = {f_stat:.4f}, p-value = {p_anova:.4f}")
# print(f"Краскела-Уоллиса: статистика = {kruskal_stat:.4f}, p-value = {p_kruskal:.4f}")



# 6.3. Коэффициенты корреляции Пирсона, Спирмена, Кендалла
# Выбираем количественные переменные
# numeric_data = data.select_dtypes(include=["int64", "float64"])

# # Для группы 1
# corr_pearson_group1 = group1[numeric_data.columns].corr(method="pearson")
# corr_spearman_group1 = group1[numeric_data.columns].corr(method="spearman")
# corr_kendall_group1 = group1[numeric_data.columns].corr(method="kendall")

# # Для группы 2
# corr_pearson_group2 = group2[numeric_data.columns].corr(method="pearson")
# corr_spearman_group2 = group2[numeric_data.columns].corr(method="spearman")
# corr_kendall_group2 = group2[numeric_data.columns].corr(method="kendall")

# print("Коэффициенты корреляции для группы 1:")
# print("Пирсон:\n", corr_pearson_group1)
# print("Спирмен:\n", corr_spearman_group1)
# print("Кендалл:\n", corr_kendall_group1)

# print("\nКоэффициенты корреляции для группы 2:")
# print("Пирсон:\n", corr_pearson_group2)
# print("Спирмен:\n", corr_spearman_group2)
# print("Кендалл:\n", corr_kendall_group2)



# 6.4. Частный коэффициент корреляции
# from pingouin import partial_corr

# # Выбираем две количественные переменные с максимальным коэффициентом корреляции Пирсона
# # Например, "возраст" и "% выполнения разработок в срок"
# var1 = "возраст"
# var2 = "% выполнения разработок в срок, в рамках бюджета, с требуемым функционалом"

# # Для группы 1
# partial_corr_group1 = partial_corr(
#     data=group1,
#     x=var1,
#     y=var2,
#     covar=["стаж работы"]
# )

# # Для группы 2
# partial_corr_group2 = partial_corr(
#     data=group2,
#     x=var1,
#     y=var2,
#     covar=["стаж работы"]
# )

# print("Частный коэффициент корреляции для группы 1:")
# print(partial_corr_group1)

# print("\nЧастный коэффициент корреляции для группы 2:")
# print(partial_corr_group2)




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
