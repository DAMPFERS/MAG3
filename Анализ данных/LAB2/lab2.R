
evaluate_model_residuals <- function(model, actual_values, label_) {
  residuals_val <- residuals(model)
  # (Min error)
  min_error <- min(residuals_val)
  # (Max error)
  max_error <- max(residuals_val)
  # (Mean error)
  mean_error <- mean(residuals_val)
  # (Std. dev.)
  std_dev_error <- sd(residuals_val)
  # (Mean absolute error)
  mean_absolute_error <- mean(abs(residuals_val))
  # (Mean percentage error)
  mean_percentage_error <- mean(residuals_val / actual_values) * 100
  # (Mean absolute percentage error)
  mean_abs_percentage_error <- mean(abs(residuals_val / actual_values) * 100)
  # (Root mean squared error)
  rmse <- sqrt(mean(residuals_val^2))

  sst <- sum((actual_values - mean(actual_values))^2)
  ssr <- sum(residuals_val^2)
  determ <- 1 - (ssr / sst)
  cat(" -- ", label_, " --\n")
  cat(
    "MIN\t\tMAX\t\tMEAN\t\tSD\t\tMAE\t\tMPE\t\tMAPE\t\tRMSE\t\tkD",
    "\n",
    min_error, "\t", max_error, "\t", mean_error, "\t", std_dev_error, "\t",
    mean_absolute_error, "\t", mean_percentage_error, "\t",
    mean_abs_percentage_error, "\t", rmse, "\t",
    determ, "\t"
  )
  return(list(
    min_error = min_error,
    max_error = max_error,
    mean_error = mean_error,
    std_dev_error = std_dev_error,
    mean_absolute_error = mean_absolute_error,
    mean_percentage_error = mean_percentage_error,
    mean_abs_percentage_error = mean_abs_percentage_error,
    rmse = rmse,
    determ = determ
  ))
}




# Имена колонок таблицы
columns.year.name <- "год"
columns.month.name <- "месяц"
columns.t.name <- "t"
columns.zptr.name <- "вар.4" # зарплата тыс.руб.



# Пункт 1

data <- read.csv("data.csv", sep = ";", fileEncoding = "CP1251")
names(data)

columns.zptr.data <- data[,columns.zptr.name]
columns.t.data <- data[,columns.t.name]




# Пункт 2

tsData <- ts(columns.zptr.data, frequency = 12, start = 2007)
# График ВР
plot.ts(tsData, main = "Source")

decomposed <- decompose(tsData)
# График основных компонентов (тренд, сезонность и шум)
plot(decomposed)

acf(columns.zptr.data, lag.max = 200, plot = TRUE, main = "ACF")
pacf(columns.zptr.data, lag.max = 200, plot = TRUE, main = "PACF")



# Пункт 3

# Модель 1
# summary()
# summary(data$t)

zptr <- columns.zptr.data
t <- columns.t.data

model1 <- nls(
  zptr ~ a *(b^t),
  data = data,
  start = list(a = 100, b = 1.01)
) # Подгонка модели

coef(model1)

summary(model1)

plot(
  t,
  zptr,
  main = "--",
  type = "l",
  xlab = "Time",
  ylab = "Value"
)
lines(t, fitted(model1), col = "red", lwd = 2)
legend(
  "topleft",
  legend = c("Source", "Model"),
  col = c("black", "red"),
  lwd = 2
)

residuals_no_trend <- residuals(model1)
plot(
  t,
  residuals_no_trend,
  type = "l",
  main = "Residuals"
)

periodogram <- spec.pgram(
  ts(residuals_no_trend, frequency = 12, start = c(2007, 1)),
  detrend = FALSE, log = "no",
  fast = FALSE, plot = TRUE,
  main = "Periodogram"
)

acf(residuals_no_trend, main = "ACF")
pacf(residuals_no_trend, main = "PACF")

m1_acc <- evaluate_model_residuals(
  model1,
#   data$zptr,
  zptr,
  "model1"
)

m1_acc

# Модель 2

model2 <- nls(
  residuals_no_trend ~
    b1 * cos(2 * pi * t / 12) + b2 * sin(2 * pi * t / 12)
      + b3 * cos(2 * 4 * pi * t / 12) + b4 * sin(2 * 4 * pi * t / 12),
  start = list(b1 = 100, b2 = 100, b3 = 50, b4 = 50)
)

coef(model2)
summary(model2)

plot(
  t,
  residuals_no_trend,
  type = "l",
  main = "Model 2",
  xlab = "Time",
  ylab = "Value",
)
lines(
  t,
  fitted(model2),
  col = "red",
  lwd = 2,
)
legend(
  "topleft",
  legend = c("Source", "Model"), col = c("black", "red"), lwd = 2
)

evaluate_model_residuals(
  model2,
  residuals_no_trend,
  "model2"
)

# Модель 3

# zptr <- data$zptr
# zptr <- columns.zptr.data
# t <- data$t



summary(zptr)
summary(t)
model3 <- nls(
    zptr ~ a *(b^t) + 
    b1 * cos(2 * pi * t / 12) + b2 * sin(2 * pi * t / 12)
    + b3 * cos(2 * 4 * pi * t / 12) + b4 * sin(2 * 4 * pi * t / 12),
    start = list(a = 100, b = 1.01,
               b1 = 100, b2 = 100, b3 = 50, b4 = 50)
)
summary(model3)

plot(
  t,
  zptr,
  type = "l",
  xlab = "Time",
  ylab = "Value",
  main = "Full original"
)
lines(
  t,
  fitted(model3),
  col = "red",
  lwd = 2,
)
legend(
  "topleft",
  legend = c("Source", "Model"), col = c("black", "red"), lwd = 2
)

final_residuals <- residuals(model3)
periodogram <- spec.pgram(
  ts(final_residuals, frequency = 12, start = c(2007, 1)),
  detrend = FALSE, log = "no", fast = FALSE,
  plot = TRUE, main = "Peridiogram full"
)

acf(final_residuals, main = "ACF final")
pacf(final_residuals, main = "PACF final")

m3_acc <- evaluate_model_residuals(
  model3,
  zptr,
  "model3"
)

m3_acc

next_three_t <- seq(from = length(t) + 1, length.out = 3, by = 1)
predict(model3, newdata = data.frame(x = next_three_t))

# data.frame(
#   ModelName = c("Trend", "Trend+Seasonal"),
#   MIN = c(m1_acc$min_error, m3_acc$min_error),
#   MAX = c(m1_acc$max_error, m3_acc$max_error),
#   MEAN = c(m1_acc$mean_error, m3_acc$mean_error),
#   STD_DEV = c(m1_acc$std_dev_error, m3_acc$std_dev_error),
#   MAE = c(m1_acc$mean_absolute_error, m3_acc$mean_absolute_error),
#   MPE = c(m1_acc$mean_percentage_error, m3_acc$mean_percentage_error),
#   MAPE = c(
#     m1_acc$mean_abs_percentage_error,
#     m3_acc$mean_abs_percentage_error
#   ),
#   RMSE = c(m1_acc$rmse, m3_acc$rmse),
#   kD = c(m1_acc$determ, m3_acc$determ)
# )





