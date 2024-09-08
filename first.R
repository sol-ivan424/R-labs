#1
x = -2.7
y = 4
z = x
x = y
y <- z

#2
x <- 3.5
y <- "2,6"
z <- 1.78
h <- TRUE
class(x)
class(y)
class(z)
class(h)
h <- as.numeric(h)
y <- sub(",",".", y)
y<- as.numeric(y)
x<- as.character(x)

#3
dohod <- 1573
dohod <- log(dohod)

#4
var_20 <- readLines(con = "task4.txt", encoding = "UTF-8")
var_20 <- 2*as.numeric(var_20) - 1
print(var_20)


#часть 2
# Задание 1
g <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)

print("Первый элемент вектора:")
print(g[1])

print("Последний элемент вектора:")
print(tail(g, n = 1))

print("Элементы вектора с третьего по пятый включительно:")
print(g[3:5])

print("Элементы вектора, которые равны 2:")
print(g[g == 2])

print("Элементы вектора, которые больше 4:")
print(g[g > 4])

print("Элементы вектора, которые кратны 3:")
print(g[g %% 3 == 0])

print("Элементы вектора, которые больше 4 и кратны 3:")
print(g[g > 4 & g %% 3 == 0])

print("Элементы вектора, которые или меньше 1, или больше 5:")
print(g[g < 1 | g > 5])

print("Индексы элементов, которые равны 0:")
print(which(g == 0))

print("Индексы элементов, которые не меньше 2 и не больше 8:")
print(which(g >= 2 & g <= 8))

g_sorted <- sort(g[g != 2], na.last = TRUE)
print("Элементы вектора по возрастанию, с пропущенными значениями в конце без цифр «2»:")
print(g_sorted)

# Задание 2
replace_last_with_na <- function(vec) {
  vec[length(vec)] <- NA
  return(vec)
}

vector <- c(1, 2, 3, 4, 5)
vector_with_na <- replace_last_with_na(vector)
print(vector_with_na)

# Задание 3
vector <- c(1, NA, 3, NA, 5, NA)

print_missing_indices <- function(vec) {
  missing_indices <- which(is.na(vec))
  if (length(missing_indices) == 0) {
    cat("Пропущенных значений нет.\n")
  } else {
    cat("Индексы пропущенных значений:", missing_indices, "\n")
  }
}

print_missing_indices(vector)
# Задание 4
vector <- c(1, 2, NA, 4, NA, 6, NA)

count_missing <- function(vec) {
  sum(is.na(vec))
}

missing_count <- count_missing(vector)
cat("Количество пропущенных значений в векторе:", missing_count, "\n")

# Задание 5
respondent_ids <- 1:100

print(respondent_ids)
# Задание 6
countries <- c("France", "Italy", "Spain")

years <- c(2019, 2020, 2020, 2018, 2017, 2019, 2020, 2020, 2018, 2017, 2019, 2020, 2020, 2018, 2017)

print(countries)
print(years)

# Задание 7 
income <- c(10000, 32000, 28000, 150000, 65000, 1573)

mean_income <- sum(income) / length(income)

income_class <- ifelse(income >= mean_income, 1, 0)

income_class

#задание 8


