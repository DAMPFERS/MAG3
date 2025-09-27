.libPaths(c("C:/PROGRAMS/MAG/MAG3/Анализ данных/LAB1/lib", .libPaths()))
# install.packages("psych") ...

library(psych)      # Библиотека для психометрического анализа, включая описание данных
library(ggm)        # Библиотека для работы с графическими моделями Гаусса (графические модели)
library(corrplot)   # Библиотека для визуализации корреляционных матриц



# Изучаются показатели работы программистов крупной организации. 
# Рассматриваются следующие показатели (признаки) для каждого программиста:
# -	пол (1-м, 2-ж);
# -	возраст;
# -	стаж работы;
# -	процент разработок, выполненных в срок в рамках бюджета 
#   с требуемым функционалом (за год);
# -	количество ошибок, выявленных пользователем (за год);
# -	стаж работы по специальности в данной организации;
# -	степень удовлетворенности заказчика;
# -	качество документирования (1 – низкое, 2 – среднее, 3 – выше среднего, 
# 4 - высокое).
# Необходимо провести предварительный разведочный анализ данных с целью описания 
# характера распределения данных, выявления структуры взаимосвязей между
# показателями. Программисты разбиты на две группы в зависимости от стажа работы. 
# Вариант 4
# 1 группа – стаж менее 5
# 2 группа - стаж более 5


data <- read.csv("./var4.csv", header=TRUE, sep=";", fileEncoding = "CP1251")                # Загружает данные из файла var4.csv с разделителем ;
data <- subset(data, select = -X.п.п)                                                        # Удаляет колонку "X.п.п"

str(data)

# Преобразует категориальные переменные в тип factor для корректного анализа.
data$группа = as.factor(data$группа)
data$пол = as.factor(data$пол)
data$качество.документирования = as.factor(data$качество..документирования)
data$степень.удовлетворенности.заказчика..качественная.оценка. = as.factor(data$степень..удовлетворенности.заказчика..качественная.оценка.)




# Создаются подмножества данных:
# group1 и group2: по стажу работы.
# gender1 и gender2: по полу
group1 <- subset(data, группа == 1)
group2 <- subset(data, группа == 2)
gender1 <- subset(data, пол == 1)
gender2 <- subset(data, пол == 2)





# names(data)
View(data)                      # Открывает таблицу данных для визуального просмотра

#View(data[c(1,3,5)])

#View(data$пол[1:90])

#str(data)

find_mode <- function(x) {   # Находит моду (наиболее часто встречающееся значение) в векторе
  
  unq_data <- unique(x)                 # Уникальные значения в векторе
  map_data <- match(x, unq_data)        # Сопоставляет значения с их индексами
  tabulate_data <- tabulate(map_data)   # Подсчитывает частоту каждого значения
  max_val <- max(tabulate_data)         # Находит максимальную частоту
  unq_data[tabulate_data == max_val]    # Возвращает значение(я) с максимальной частотой
}


find_mode_df <- function(x) {               # Функция для поиска моды по всем колонкам
  print ("Мода для каждой колонки \n")
  for (i in 2 : ncol(x)){
    # calculating mode of ith column
    mod_val <- find_mode(x[,i]) # Вычисляет моду для i-й колонки
    name <- colnames(x)[i]      # Имя колонки
    print (c(name,": ", mod_val), collapse=" ")
  }
}


# 3. основные статистические характеристики по количественным данным 
# (минимальное, максимальное, среднее значение, стандартное отклонение, 
# первый и третий квартили, медиана, мода, асимметрия, эксцесс) 
# отдельно для первой и второй групп и для всей выборки.
# mean - среднее значение
# min - минимальное значение
# max - максимальное значение
# sd - standard deviation - стандартное отклонение
# median - медиана, 50-й квантиль, 50% выборки имеют значение меньше, 50% больше
# trimmed - среднее значение после удаления некоторого % 
#           самых малых и больших значений из выборки
# mad - median absolute deviation, абс. отклонение медианы 
# range - размах
# skew - ассиметрия распределения вероятности
# kurtosis - эксцесс, мера интенсивности выбросов относительно нормального распределения
# se - standard error, стандартная ошибка, мера неопределенности данного критерия
# первый квартиль - 25-й квантиль
# третий квартиль - 75-й квантиль
# мода - наиболее часто встречающееся значение

View(describe(data))
View(describe(group1))
View(describe(group2))

# квартили
View(summary(data))
View(summary(group1))
View(summary(group2))

print("Для всей выборки")
find_mode_df(data)

print("Для группы 1")
find_mode_df(group1)

print("Для группы 2")
find_mode_df(group2)

#plot(data.frame(data$количество.ошибок..выявленных.пользователем,data$стаж))

#boxplot(data$стаж)


# 4.

# 4.1 График рассеяния
plot(
  data.frame(data$возраст,data$стаж.работы), 
  main="График рассеяния",
  xlab = "Возраст", ylab = "Стаж работы",
  pch = 10,
  xaxp = round(c(min(data$возраст), max(data$возраст),20)),
  yaxp = round(c(min(data$стаж.работы), max(data$стаж.работы),8))
)


4.2 Радиальный график для категориальных данных
drawPieCat <- function(column, main_label) {
#   freqs <- rle(sort(column))
  freqs <- rle(sort(as.vector(column)))

  
  cat_var <- factor(c(
    rep(freqs$values, freqs$lengths)
  ))
  
  cat <- table(cat_var)
#   colname <- colnames(column)[0]
  pie(
    cat, radius=1, 
    main=main_label, 
    col = hcl.colors(length(cat), "BluYl"),
    clockwise=TRUE,
    labels = paste0(round(100 * cat/sum(cat), 2), "%")
  )
  legend(
    "topright", 
    c(freqs$values), 
    cex=0.8, fill=hcl.colors(length(cat), "BluYl")
  )
}

par(mfrow = c(1, 1)) 
drawPieCat(
  data$степень.удовлетворенности.заказчика..качественная.оценка.,
  "Степень удовлетворенности заказчика"
);

# 4.3
drawPieCat(
  group1$степень.удовлетворенности.заказчика..качественная.оценка.,
  "Степень удовлетворенности заказчика, группа 1"
);
drawPieCat(
  group2$степень.удовлетворенности.заказчика..качественная.оценка.,
  "Степень удовлетворенности заказчика, группа 2"
);
drawPieCat(
  gender1$степень.удовлетворенности.заказчика..качественная.оценка.,
  "Степень удовлетворенности заказчика, пол 1"
);
drawPieCat(
  gender2$степень.удовлетворенности.заказчика..качественная.оценка.,
  "Степень удовлетворенности заказчика, пол 2"
);

# 4.4 Стобиковая диаграмма

par(mfrow = c(1, 1))
qualityInGroup1 = mean(group1$степень..удовлетворенности.заказчика..балльная.оценка.)
qualityInGroup2 = mean(group2$степень..удовлетворенности.заказчика..балльная.оценка.)

qualityInGender1 = mean(gender1$степень..удовлетворенности.заказчика..балльная.оценка.)
qualityInGender2 = mean(gender2$степень..удовлетворенности.заказчика..балльная.оценка.)

quality = c(qualityInGroup1, qualityInGroup2, qualityInGender1, qualityInGender2)

barplot(
  quality,
  main = "Средняя степень удовлетворенности заказчика (балльная оценка)",
  names.arg = c("Группа 1", "Группа 2", "Пол 1", "Пол 2"),
  col = hcl.colors(length(quality), "BluYl"),
  xlab = "", ylab = "Степень удовлетворенности заказчика (балльная оценка)"
)

# 4.5
par(mfrow = c(1, 1)) 
boxplot(
  X..выполнения.разработок.в.срок..в.рамках.бюджета..с.требуемым.функционалом ~ группа, 
  main="% выполнения разработок в срок", ylab="% выполнения разработок в срок",
  data=data,
  ylim = c(
    min(
      data$X..выполнения.разработок.в.срок..в.рамках.бюджета..с.требуемым.функционалом)-1, 
    max(
      data$X..выполнения.разработок.в.срок..в.рамках.бюджета..с.требуемым.функционалом)+1
  )
)


# 4.6
par(mfrow = c(3, 2))
hist(
  data$X..выполнения.разработок.в.срок..в.рамках.бюджета..с.требуемым.функционалом,
  main="% выполнения разработок в срок c требуемым функционалом",
  xlab=""
)
hist(
  data$возраст,
  main="Возраст",
  xlab=""
)
hist(
  data$стаж.работы,
  main="Стаж работы",
  xlab=""
)
hist(
  data$количество.ошибок..выявленных.пользователем,
  main="Количество ошибок, выявленных пользователем",
  xlab=""
)
hist(
  data$степень..удовлетворенности.заказчика..балльная.оценка.,
  main="Степень удовлетворенности заказчика (балльная оценка)",
  xlab=""
)

# 4.7
str(data)
par(mfrow = c(1, 1))
pairs(
  ~X..выполнения.разработок.в.срок..в.рамках.бюджета..с.требуемым.функционалом + 
  возраст + 
  стаж.работы +
  количество.ошибок..выявленных.пользователем +
  степень..удовлетворенности.заказчика..балльная.оценка.,
  data=data, main="Матричный график",
  cex.labels=1.5,
  cex.axis=1.5
)



# 6.1
# Chi squared
print("Группа 1")
print(chisq.test(
  table(
    group1$пол,
    group1$степень.удовлетворенности.заказчика..качественная.оценка.
  ),
  correct=TRUE
))

print("Группа 2")
print(chisq.test(
  table(
    group2$пол,
    group2$степень.удовлетворенности.заказчика..качественная.оценка.
  ),
  correct=TRUE
))

# Fisher
print("Группа 1")
print(fisher.test(
  table(
    group1$пол,
    group1$степень.удовлетворенности.заказчика..качественная.оценка.
  )
))

print("Группа 2")
print(fisher.test(
  table(
    group2$пол,
    group2$степень.удовлетворенности.заказчика..качественная.оценка.
  )
))



# 6.2 ANOVA
# различаются ли средние значения между группами

aov_model <- aov(
  X..выполнения.разработок.в.срок..в.рамках.бюджета..с.требуемым.функционалом ~ 
    степень.удовлетворенности.заказчика..качественная.оценка., 
  data=data
)
summary(aov_model)
# https://www.scribbr.com/statistics/anova-in-r/
#df = the number of levels in the variable minus 1
# residuals df = the total number of observations minus one and minus 
# the number of levels in the independent variables
# Pr(>F) - p value. This shows how likely it is that the F value calculated
# from the test would have occurred if the null hypothesis of no difference 
# among group means were true.
# у нас p = <2e-16 < 0.05, что говорит о наличии связи между группой и
# cтепенью удовлетворенности заказчика


# 6.3 Рассчет коэффициентов корреляции, 5.5
# Линейный коэффициент корреляции Пирсона (Pearson product moment correlation)
# отражает степень линейной связи между двумя количе-ственными переменными. 
# Коэффициент ранговой корреляции Спирмана (Spearman’s Rank Order correlation) 
# – мера связи между двумя ранжиро-ванными переменными. 
# Тау Кендалла – также непараметрический показатель ранговой корреляции.

drawMatrix <- function(N, main_label) {
  par(mfrow = c(1, 1))
  names <- c(
    'пол', 
    # 'возраст', 
    'стаж', 
    '% вып. раб.', 
    'колво ош.', 
    'ст. удовл.', 
    'кач-во док.'
  )
  rownames(N) <- names
  colnames(N) <- names
  corrplot(
    N, method="color", 
    addCoef.col = 1, 
    tl.cex = 0.8, number.cex = 0.8,
    tl.col="black",
    main=main_label,
    mar=c(0,0,1,0)
  )
}

# M <- data[,unlist(lapply(data, is.numeric))]


# Выбираем числовые столбцы И добавляем столбец 'группа' из исходных данных
numeric_cols <- unlist(lapply(data, is.numeric))
M <- data[, c("группа", names(data)[numeric_cols])]  # Добавляем группу

# Проверяем структуру
print("Структура M:")
str(M)

# Разделяем на группы (группа должна быть фактором)
M$группа <- as.factor(M$группа)

# Правильное разделение на группы
M1 <- M[M$группа == 1, -1]  # Исключаем столбец группы
M2 <- M[M$группа == 2, -1]  # Исключаем столбец группы

# Преобразуем все в числовой формат (на всякий случай)
M1 <- as.data.frame(lapply(M1, as.numeric))
M2 <- as.data.frame(lapply(M2, as.numeric))

# Проверяем
print("Размерность M1:")
print(dim(M1))
print("Типы данных в M1:")
print(sapply(M1, class))






#View(M1)
print("Группа 1")
N1 <- cor(M1,use="pairwise.complete.obs", method="pearson")
N2 <- cor(M1,use="pairwise.complete.obs", method="spearman")
N3 <- cor(M1,use="pairwise.complete.obs", method="kendall")

#print("Pearson")
#print(N1)
#print("Spearman")
#print(N2)
#print("Kendall")
#print(N3)

drawMatrix(N1, "Pearson, группа 1")
drawMatrix(N2, "Spearman, группа 1")
drawMatrix(N3, "Kendall, группа 1")

print("Группа 2")
N1 <- cor(M2,use="pairwise.complete.obs", method="pearson")
N2 <- cor(M2,use="pairwise.complete.obs", method="spearman")
N3 <- cor(M2,use="pairwise.complete.obs", method="kendall")

#print("Pearson")
#print(N1)
#print("Spearman")
#print(N2)
#print("Kendall")
#print(N3)

drawMatrix(N1, "Pearson, группа 2")
drawMatrix(N2, "Spearman, группа 2")
drawMatrix(N3, "Kendall, группа 2")



# 6.4
#View(M1)
#View(M2)

#M$степень.удовлетворенности.заказчика..балльная.оценка.
#M$X..выполнения.разработок.в.срок..в.рамках.бюджета..с.требуемым.функционалом


# if (!require("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")

# BiocManager::install("graph")

# print(pcor(c(4,6,  1,2,3,5,7), cov(M1)))
print(pcor(c(3, 5, 1, 2, 4, 6), cov(M1)))
print(pcor(c(3, 5, 1, 2, 4, 6), cov(M2)))


#
print("Pearson test")
print(cor.test(
  M1$степень..удовлетворенности.заказчика..балльная.оценка., 
  M1$X..выполнения.разработок.в.срок..в.рамках.бюджета..с.требуемым.функционалом,
  method="pearson"
))
print(cor.test(
  M2$степень..удовлетворенности.заказчика..балльная.оценка., 
  M2$X..выполнения.разработок.в.срок..в.рамках.бюджета..с.требуемым.функционалом,
  method="pearson"
))
