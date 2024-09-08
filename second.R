#часть 1
#задание 1
#заполняем матрицу 3*4 (3 строки, 4 столбца)тройками
matr <- matrix(3, nrow = 3, ncol = 4)

matr[1, 3] <- 4
matr[2, 1] <- 1
matr[3, 2] <- NA
matr[3, 4] <- 1

print(matr)

#задание 2
#c() - это вектор
a <- c(1, 3, 4, 9, NA)
b <- c(5, 6, 7, 0, 2)
c <- c(9, 10, 13, 1, 20)

#matrix()здесь принимает на вход вектор данных и число строк
#опционально может принимать аргумент byrow, который определяет, 
#в каком порядке заполняются строки матрицы:
#по столбцам (byrow = FALSE, по умолчанию) или по строкам (byrow = TRUE).
matr_cols <- matrix(c(a, b, c), nrow = length(a), byrow = FALSE)

#называем строки и столбцы
rownames(matr_cols) <- c("a", "b", "c", "d", "e")
colnames(matr_cols) <- c("Col1", "Col2", "Col3")

print(matr_cols)

#matrix() принимает на вход вектор и число столбцов
matr_rows <- matrix(c(a, b, c), ncol = length(a), byrow = TRUE)

rownames(matr_rows) <- c("a", "b", "c")
colnames(matr_rows) <- c("Col1", "Col2", "Col3", "Col4", "Col5")

print(matr_rows)

#задание 3
names <- c("Jane", "Michael", "Mary", "George")
ages <- c(8, 6, 28, 45)
gender <- c(0, 1, 0, 1)

#создание датафрейма - таблицы,  в которой могут быть разные
#типы данных (в отличие от матрицы)
df <- data.frame(Names = names, Ages = ages, Gender = gender)
print(df)

#если у нас есть датафрейм df и мы хотим получить доступ к
#столбцу Ages, мы можем написать df$Ages
#так получим вектор со значениями столбца Ages из датафрейма df

#также можно использовать $ для создания новых столбцов в датафрейме

#добавление столбца age_sq
df$age_sq <- df$Ages^2

print(df)

#задание 4
names <- c("Jane", "Michael", "Mary", "George")
ages <- c(8, 6, 28, 45)
gender <- c(0, 1, 0, 1)

#список info с тремя элементами: name, age и gender
#каждый элемент списка является вектором
info <- list(name = names, age = ages, gender = gender)

#обращаемся к вектору name и выводим его 2 элемент
print(info$name[2])

#обращаемся к вектору gender и выводим его
print(info$gender)

#обращаемся к вектору name и выводим его
print(info$name)

#добавляем новый элемент drinks
info$drinks <- c("juice", "tea", "rum", "coffee")
print(info)

#добавляет в вектор name в конце "John"
info$name <- c(info$name, "John")

info$age <- c(info$age, 2)
info$gender <- c(info$gender, 1)
info$drinks <- c(info$drinks, "milk")
print(info)

#чтоб вывод списка был красивым, делаем это:
cat(names(info), "\n")
cat(info$name, "\n")
cat(info$age, "\n")
cat(info$gender, "\n")
cat(info$drinks, "\n")

#задание 5
index <- "0,72;0,38;0,99;0,81;0,15;0,22;0,16;0,4;0,24"

#меняем запятые на точки
index_numeric <- gsub(",", ".", index)

#strsplit() разделяет строку с помощью разделителя ";"
#unlist() преобразует список в вектор
#as.numeric() преобразует вектор в числовой вектор
I <- as.numeric(unlist(strsplit(index_numeric, ";")))

print(I)

#часть 2
#задание 1
#создает квадратную матрицу размером 2*2, где главная диагональ
#содержит вектор c(4,9)
A <- diag(c(4, 9), nrow = 2, ncol = 2)


print(A)

#задание 2

#для вычисления собственных значений и собственных векторов числовых
#используется функция eigen()


#eigen(A)$values - вывод собственных значений
#eigen(A)$vectors - вывод собственный векторов

e <- eigen(A)$values
print(e)

#собственные значения для диагональной матрицы
#просто равны ее диагональным элементам

#задание 3

#создаем единичную матрицу размером 2*2
I <- diag(2)

#вычисляем разность матриц (B - тоже матрица)
B <- I - A
print(B)

#задание 4

#задаем векторы
f <- c(4, 2)
u <- c(0.2, -0.3)

cat("Вектор f:", f, "\n")
cat("Вектор u:", u)

#задание 5

#есть уравнение A*u=f
#умножаем обе части на A^(-1), то есть solve(A) - обратная матрица к А
#получаем A*A^(-1)*u=f*A^(-1), то есть u=A^(-1)*f, u=solve(A)*f 

u_result <- solve(A) %*% f

cat("Решение системы линейных уравнений:", u_result)

#задание 6

for (i in 1:7) {
  u<- B %*% u + f
  
  #создание переменной для хранения результата (u1, u2, ... u7)
  assign(paste0("u", i), u)
  #assign присваивает определенных значения нескольким новым переменных
  #paste0() объединяет строки, не используя пробел в качестве разделителя по умолчанию (paste использует)
  
  cat("Итерация", i, ": u =", u, "\n")
}


#задание 7

#сравниваем результаты u7 и u_result
difference <- u7 - u_result

print(difference)

#задание 8

#максимальный элемент матрицы А (равен 9)
max_A <- max(A)

#деление всех элементов матрицы A на max_A
A_norm <- A / max_A

#деление всех элементов вектора f на max_A
f_norm <- f / max_A

print(A_norm)
print(f_norm)

#задание 9

#собственные значения
e_norm <- eigen(A_norm)$values
print(e_norm)

#матрица B_norm = I - A_norm
B_norm = I - A_norm
print(B_norm)

#решение СЛАУ для A_norm
u_result_norm <- solve(A_norm) %*% f
cat("Решение системы линейных уравнений:", u_result_norm)

#итерации по данной схеме
for (i in 1:7) {
  u <- B_norm %*% u + f
  #создание переменной для хранения результата (u_norm, ... u_norm7)
  assign(paste0("u_norm", i), u)
  
  cat("Итерация", i, ": u_norm =", u, "\n")
}

#находим разницу между u_norm7 и u_result_norm
difference_norm <- u_norm7 - u_result_norm
print(difference_norm)

#задание 10

print(u7 - u_norm7)
print(u_result - u_result_norm)



#часть 3
step <- 1
dekart_begin <- -5
dekart_end <- 5
x <- seq(from = dekart_begin, to = dekart_end, by = step)
y <- x 
surface_matrix <- outer(X = x, Y = y, FUN = function(x,y) Re(exp(-1i * 0.5  * x * y)))
dimnames(surface_matrix) <- list(x, y)

#задание 1
file_path <- "summary.txt"
file_conn <- file(file_path, "w")

#количество элементов матрицы 
cat("number of matrix elements:", length(surface_matrix), "\n", file = file_conn)
#размерность строк
cat("number of rows:", nrow(surface_matrix), "\n", file = file_conn)
#размерность столбцов
cat("number of cols:", ncol(surface_matrix), "\n", file = file_conn)
#сумма элементов главной диагонали
cat("sum of main diag elements:", sum(diag(surface_matrix)), "\n", file = file_conn)
#сумма элементов серединного среза матрицы по строкам
cat("sum of middle row elements:", sum(surface_matrix[nrow(surface_matrix) %/% 2, ]), "\n", file = file_conn)
#сумма элементов серединного среза матрицы по столбцам
cat("sum of middle column elements:", sum(surface_matrix[, ncol(surface_matrix) %/% 2]), "\n", file = file_conn)
#суммы строк матрицы
cat("row sums:", colSums(surface_matrix), "\n", file = file_conn)
#суммы столбцов матрицы
cat("col sums:", rowSums(surface_matrix), "\n", file = file_conn)

close(file_conn)



#задание 2
#консольный ввод параметров координатной сетки
dekart_begin <- as.numeric(readline(prompt = "Введите начало сетки: "))
dekart_end <- as.numeric(readline(prompt = "Введите конец сетки: "))
step <- as.numeric(readline(prompt = "Введите шаг сетки: "))

# Задание сеточной поверхности
x <- seq(from = dekart_begin, to = dekart_end, by = step)
y <- x

# Задание двумерной функции на координатной сетке
surface_matrix <- outer(X = x,
                        Y = y,
                        FUN = function(x, y) Re(exp(-1i * 0.5  * x * y)))

dimnames(surface_matrix) <- list(x, y)


file_path <- "summary2.txt"
file_conn <- file(file_path, "w")

# Создаем матрицу и записываем информацию в файл

cat("number of matrix elements:", length(surface_matrix), "\n", file = file_conn)
cat("number of rows:", nrow(surface_matrix), "\n", file = file_conn)
cat("number of cols:", ncol(surface_matrix), "\n", file = file_conn)
cat("sum of main diag elements:", sum(diag(surface_matrix)), "\n", file = file_conn)
cat("row sums:", colSums(surface_matrix), "\n", file = file_conn)
cat("col sums:", rowSums(surface_matrix), "\n", file = file_conn)

close(file_conn)

#задание 3
input_file <- "C:/Users/Ramil/Desktop/inputs.txt"
input_data <- as.numeric(readLines(input_file))

# Создание сетки
x <- seq(input_data[1], input_data[2], by = input_data[3])
y <- seq(input_data[4], input_data[5], by = input_data[6])

#surface_matrix <- outer(X = x, Y = y, FUN = function(x, y) exp(-(x - 0.5)^2 / 0.01 - (y - 0.5)^2 / 0.01))
surface_matrix <- outer(X = x, Y = y, FUN = function(x, y) Re(exp(-1i * 0.5 * x * y)))
dimnames(surface_matrix) <- list(x, y)


file_path <- "summary3.txt"
file_conn <- file(file_path, "w")

cat("number of matrix elements:", length(surface_matrix), "\n", file = file_conn)
cat("number of rows:", nrow(surface_matrix), "\n", file = file_conn)
cat("number of cols:", ncol(surface_matrix), "\n", file = file_conn)
cat("sum of main diag elements:", sum(diag(surface_matrix)), "\n", file = file_conn)
cat("row sums:", colSums(surface_matrix), "\n", file = file_conn)
cat("col sums:", rowSums(surface_matrix), "\n", file = file_conn)


close(file_conn)

#часть 4
#задание 1

#устанавливаем нужную библиотеку
if ("car" %in% installed.packages() == FALSE) {
  install.packages("car")
}
#обращаемся к ней
library(car)

#формируем данные в матрицу
cars_matrix <- as.matrix(cars)
print(cars_matrix)

#отделяем первый столбец матрицы cars_matrix
first_col <- cars_matrix[, 1]

#создаем единичный вектор той же длины, что и first_col
vector <- rep(1, length(first_col))

#создаем новую матрицу, состоящую из vector и first_col
cars_speed <- cbind(vector, first_col)

print(cars_speed)



#задание 2

#срез матрицы cars_matrix по второму столбцу
cars_dist <- cars_matrix[, 2]

print(cars_dist)



#задание 3

#рассчитаем новый вектор alpha
#по следующему соотношению, называемому нормальным уравнением модели регрессии
alpha <- solve(t(cars_speed) %*% cars_speed) %*% t(cars_speed) %*% cars_dist

print(alpha)



#задание 4

#проверяем тип данных вычисленного вектора alpha
print(class(alpha))

#производим преобразование данной переменной к структуре данных “vector”
if (!is.vector(alpha)) {
  alpha <- as.vector(alpha)
}

print(alpha)



#задание 5

#создаем переменные alpha_c и alpha_x
#на основе первого и второго элемента вектора alpha соответственно
alpha_c <- alpha[1]
alpha_x <- alpha[2]

#выводим по шаблону
cat('"', "alpha_c = ", alpha_c, '"', '- первый элемент вектора', sep = '', "\n")
cat('"', "alpha_x = ", alpha_x, '"', '- второй элемент вектора', sep = '', "\n")



#задание 6

#новый вектор cars_speed_lm на основе
#матрицы cars_matrix с помощью взятия его первого столбца
cars_speed_lm <- cars_matrix[, 1]

print(cars_speed_lm)



#задание 7

#создаем новый вектор cars_dist_lm на основе данного соотношения
cars_dist_lm <- alpha_c + cars_speed_lm * alpha_x

print(cars_dist_lm)



#задание 8

#в вектор dist_residuals сохраняем информацию о 
#разности между векторами cars_dist_lm и cars_dist. 
dist_residuals <- cars_dist_lm - cars_dist

print(dist_residuals)



#задание 9

#вычисляем среднее отклонение вектора dist_residuals 
mean_value <- mean(dist_residuals)

#вычисляем стандартное отклонение вектора dist_residuals 
sd_value <- sd(dist_residuals)

cat("Среднее отклонение dist_residuals:", mean_value, "\n")
cat("Стандартное отклонение dist_residuals:", sd_value, "\n")



#задание 10

#вывести на экран значения вектора cars_dist_lm
#убедиться в их отсортированности по возрастанию

cat("Значения вектора cars_dist_lm без сортировки:\n")
print(cars_dist_lm)

#действительно, значения расположены по возрастанию



#задание 11

#выводим среднее и стандартное отклонения на экран
cat("Среднее значение dist_residuals:", mean_value, "\n")
cat("Стандартное отклонение dist_residuals:", sd_value, "\n")