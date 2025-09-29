# Подготовка среды
.libPaths(c("C:/PROGRAMS/MAG/MAG3/Анализ данных/LAB3/lib", .libPaths()))

#Установка библиотек:

# install.packages("arules")
# install.packages("arulesViz")
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)


# Параметры по умолчанию:
lwd <- 4
font <- 2


# Подготовка данных

# Загрузка исходных данных:
data <- read.delim(
  file = "lab3_4.csv",
  sep = ",",
  header = TRUE,
  row.names = 1
)
names(data)

# Поиск ассоциативных правил будет проводиться для записей с использованием кредита, поэтому необходимо отделить столбец «Кредит»:

dataN <- data[, -11]
isCredit <- data$Credit
isCredit[isCredit == 1] <- "Credit"
isCredit[isCredit == 0] <- "NoCredit"


# Преобразования исходных данных в файл транзакций:

itemsList <- sapply(
  1:nrow(dataN),
  function(i) paste(
    c(isCredit[i], colnames(dataN[i, dataN[i,] == 1])),
    collapse = ",",
    sep = "\n"
  )
)

head(itemsList)


# Заполнение файла транзакций:

write(itemsList, file = "lab3_4_basket.csv")


# Проведения ассоциативного анализа

# Считывание записанных ранее транзакций из файла:
trans <- read.transactions(
  file = "lab3_4_basket.csv",
  format = "basket",
  sep = ","
)

summary(trans)


# Построение частотной диаграммы транзакций:



# Построение частотной диаграммы транзакций:

plot.new()
par(lwd = lwd, font = font)

# Используем правильное имя столбца "Кредит" вместо "Credit"
print("Правильные имена столбцов:")
print(names(data))

# Создаем данные для графика из исходного data.frame
# Разделяем данные на две группы: с кредитом и без
data_credit <- data[data$Кредит == 1, ]    # С кредитом
data_no_credit <- data[data$Кредит == 0, ] # Без кредита

print(paste("Транзакций с кредитом:", nrow(data_credit)))
print(paste("Транзакций без кредита:", nrow(data_no_credit)))

# Суммируем покупки по товарам для каждой группы (исключаем столбец Кредит)
credit_counts <- colSums(data_credit[, -11])  # Исключаем столбец Кредит
no_credit_counts <- colSums(data_no_credit[, -11])

print("Покупки с кредитом:")
print(credit_counts)
print("Покупки без кредита:")
print(no_credit_counts)

# Создаем данные для графика
item_data <- data.frame(
  Item = rep(names(credit_counts), 2),
  Count = c(credit_counts, no_credit_counts),
  Status = rep(c("Credit", "NoCredit"), each = length(credit_counts))
) %>%
  group_by(Item) %>%
  mutate(Total = sum(Count)) %>%
  arrange(desc(Total)) %>%
  ungroup()

print("Данные для графика:")
print(item_data)

# Построение графика
ggplot(item_data, aes(x = reorder(Item, -Total), y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Товары", y = "Количество покупок", 
       title = "Частота покупок товаров с кредитом и без") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 16),
    legend.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 14)
  ) +
  scale_fill_manual(values = c("Credit" = "steelblue", "NoCredit" = "darkorange"))



itemLabels(trans)

# Составления ассоциативных правил с минимальной поддержкой 0.1 и минимальной достоверностью 0.5:

# Пересоздадим транзакции с включением информации о кредите
itemsList_with_credit <- sapply(
  1:nrow(data),
  function(i) {
    # Получаем товары, которые были куплены
    purchased_items <- colnames(data[i, -11])[data[i, -11] == 1]
    
    # Добавляем информацию о кредите
    if(data[i, 11] == 1) {
      all_items <- c(purchased_items, "Credit")
    } else {
      all_items <- c(purchased_items, "NoCredit")
    }
    paste(all_items, collapse = ",")
  }
)

# Записываем в файл
write(itemsList_with_credit, file = "lab3_4_basket_with_credit.csv")

# Считываем новые транзакции с информацией о кредите
trans_with_credit <- read.transactions(
  file = "lab3_4_basket_with_credit.csv",
  format = "basket",
  sep = ","
)

# Проверим, что теперь есть элементы Credit и NoCredit
print("Элементы в новых транзакциях:")
print(itemLabels(trans_with_credit))

# Составление ассоциативных правил с минимальной поддержкой 0.1 и минимальной достоверностью 0.5:
rules <- apriori(trans_with_credit, parameter = list(support = 0.1, confidence = 0.5))

# Построение сети ассоциативных правил, которые содержат транзакции с использованием кредита:
plot.new()
par(lwd = lwd, font = font)
rulesWithCredit <- subset(rules, subset = rhs %in% "Credit")
plot(rulesWithCredit, method = "paracoord")

# Граф ассоциативных правил:
plot.new()
plot(
  rulesWithCredit,
  method = "graph",
  control = list(
    nodeCol = grey.colors(10),
    edgeCol = grey(.7),
    alpha = 1
  )
)

inspect(sort(rulesWithCredit, by = "support"))


rules <- apriori(trans, parameter = list(support = 0.1, confidence = 0.5))


# Построение сети ассоциативных правил, которые содержат транзакции с использованием кредита:

plot.new()
par(lwd = lwd, font = font)
rulesWithCredit <- subset(rules, subset = rhs %in% "Credit")
plot(rulesWithCredit, method = "paracoord")


# Граф ассоциативных правил:

plot.new()
plot(
  rulesWithCredit,
  method = "graph",
  control = list(
    nodeCol = grey.colors(10),
    edgeCol = grey(.7),
    alpha = 1
  )
)

inspect(sort(rulesWithCredit, by = "support"))