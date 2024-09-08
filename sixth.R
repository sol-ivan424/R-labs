##################задание 1##################

if ("quantmod" %in% rownames(installed.packages()) == FALSE) {
  install.packages("quantmod")
}
library(quantmod)

if ("stringr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("stringr")
}
library(stringr)

#загрузка акций в yahoo
downloadable_stocks <- c("TSLA", "ATVIX")

#получение фреймов с данными
quantmod::getSymbols(Symbols = downloadable_stocks, src = "yahoo", from = as.Date.character("1900-01-01"))

#downloadable_stocks[1] - акции TSLA, downloadable_stocks[1] - акции ATVIX
df <- data.frame(get(downloadable_stocks[2]))


##################задание 2##################

out_of_trend <- function(x, dt, method = c("Arifm", "Geom", "Garm")) {
  # Проверка входных данных
  if (!is.numeric(x) || length(x) < 3) {
    stop("Вектор должен быть числовым и иметь длину не менее 3")
  }
  if (!is.numeric(dt) || length(dt) != 1) {
    stop("Пробное смещение dt должно быть числом")
  }
  if (dt > ceiling(length(x) / 2) - 1) {
    stop("dt не должно превышать ceiling(length(x)/2) - 1")
  }
  
  method <- match.arg(method)
  
  # Подъем вектора по оси ординат
  min_val <- min(x)
  x <- x - min_val + 1
  
  n <- length(x)
  yt <- numeric(n - 2 * dt)
  
  for (t in (dt + 1):(n - dt)) {
    if (method == "Arifm") {
      yt[t - dt] <- log((x[t - dt] + x[t + dt]) / (2 * x[t]))
    } else if (method == "Geom") {
      yt[t - dt] <- log((x[t - dt] * x[t + dt]) / (x[t]^2))
    } else if (method == "Garm") {
      yt[t - dt] <- log((2 * x[t - dt] * x[t + dt]) / (x[t] * (x[t - dt] + x[t + dt])))
    }
  }
  
  return(yt)
}


x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
dt <- 2
method <- "Arifm"
result <- out_of_trend(x, dt, method)
print(result)


##################задание 3##################

#задаем векторы t и x
t <- seq(0, 10, 0.1)
x <- 2 * t + 3 + sin(2 * t)

#среднее значение вектора x
mean_x <- mean(x)
print(paste("Среднее значение x:", mean_x))

dt <- 2
method <- "Arifm"
xn <- out_of_trend(x, dt, method)

#среднее значение вектора xn
mean_xn <- mean(xn)
print(paste("Среднее значение xn:", mean_xn))

cat("Среднее значение xn находится около нуля, потому что при исключении тренда из ряда мы устраняем основную компоненту,\nкоторая определяет общий уровень ряда. Оставшиеся значения представляют собой отклонения от тренда,\nкоторые в среднем балансируют вокруг нуля.")


##################задание 4##################

alter_johns <- function(y) {
  n <- length(y)
  t_max <- floor(n / 2)
  
  a_t <- numeric(t_max)
  
  for (t in 1:t_max) {
    sum_val <- 0
    for (i in 1:(n - t)) {
      sum_val <- sum_val + abs(y[i + t] - y[i])
    }
    a_t[t] <- sum_val / (n - t)
  }
  
  return(a_t)
}


##################задание 5##################

#пприменяем функцию Альтера-Джонса к вектору xn
aj_result <- alter_johns(xn)

#локальный минимум

#используется функция which.min для поиска индекса, при котором функция Альтера-Джонса достигает своего минимума.
#этот индекс соответствует смещению t, при котором наблюдается наименьшее значение функции Альтера-Джонса, что указывает на периодичность.
t_min <- which.min(aj_result)
print(paste("Смещение t, соответствующее локальному минимуму функции Альтера-Джонса:", t_min))

print(aj_result)


##################задание 6##################

#вбираем колонку Adjusted (скорректированная цена закрытия)
column_name <- "ATVIX.Adjusted"
x <- df[[column_name]]

#применение функции удаления тренда к колонке данных x
dt <- 2
method <- "Arifm"
xn <- out_of_trend(x, dt, method)

#применение функции Альтера-Джонса к вектору xn
aj_result <- alter_johns(xn)


#сохранение результата работы функции Альтера-Джонса в отдельный вектор
aj_result_vector <- aj_result

print(aj_result_vector)


#визуализация результата
plot(aj_result_vector, type = "o", col = "blue", main = "Функция Альтера-Джонса",
     xlab = "Смещение t", ylab = "Значение функции")

#локальный минимум
t_min <- which.min(aj_result)
print(paste("t_min", t_min))

#нанесение первого локального минимума на график
points(t_min, aj_result_vector[t_min], col = "red", pch = 19)
text(t_min, aj_result_vector[t_min], labels = paste("Min (t =", t_min, ")"), pos = 3, col = "red")