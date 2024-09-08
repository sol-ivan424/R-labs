#задание 1

# Установка текущей даты
current_date <- Sys.Date()

# Создание диапазона значений x
x <- seq(-10, 10, by = 0.1)

# Вычисление значений функции y = x^2
y <- x^2

# Построение графика функции y = x^2
plot(x, y, type = "l", col = "blue", xlab = "x", ylab = "y = x^2", main = "График функции y = x^2")

# Нанесение текущей даты на график
text(8, 80, paste("Текущая дата:", current_date), adj = 1)

# Добавление сетки
grid()

#задание 2
 ##первое подзадание
# Функция для ввода данных об успеваемости студентов
input_grades <- function() {
  students <- c("Student1", "Student2", "Student3", "Student4")
  subjects <- c("Math", "Physics", "Chemistry")
  grades <- matrix(nrow=length(students), ncol=length(subjects))
  
  for (i in 1:length(students)) {
    for (j in 1:length(subjects)) {
      repeat {
        grade <- as.numeric(readline(paste("Введите оценку для", students[i], "по предмету", subjects[j], ": ")))
        if (!is.na(grade) && grade >= 0 && grade <= 100) {
          grades[i, j] <- grade
          break
        } else {
          cat("Пожалуйста, введите корректную оценку (число от 0 до 100).\n")
        }
      }
    }
  }
  
  colnames(grades) <- subjects
  rownames(grades) <- students
  return(grades)
}

# Ввод данных
grades <- input_grades()

# Выводим данные для проверки
print("Данные об успеваемости студентов:")
print(grades)

# Извлекаем данные успеваемости третьего студента
student3_grades <- grades[3, ]

# Построение круговой диаграммы успеваемости третьего студента
pie(student3_grades, labels = names(student3_grades), main = "Успеваемость третьего студента")

 ##второе подзадание

if ("plotrix" %in% installed.packages() == FALSE) {
  install.packages("plotrix")
}



# Ввод количества студентов и предметов
num_students <- as.integer(readline("Введите количество студентов (до 5): "))
num_subjects <- as.integer(readline("Введите количество предметов (до 25): "))

# Проверка на допустимость введенных значений
if (num_students <= 0 || num_students > 5 || num_subjects <= 0 || num_subjects > 25) {
  cat("Недопустимое количество студентов или предметов. Введите значения от 1 до 5 для студентов и от 1 до 25 для предметов.\n")
  quit(save = "no", status = 1)
}

# Создание списка студентов
students <- paste("Студент", 1:num_students)

# Создание списка дисциплин
subjects <- paste("Предмет", 1:num_subjects)

# Создание матрицы для хранения данных об успеваемости
grades <- matrix(0, nrow = num_students, ncol = num_subjects)

# Ввод данных об успеваемости
for (i in 1:num_students) {
  cat("Введите оценки для", students[i], "\n")
  for (j in 1:num_subjects) {
    grade <- as.numeric(readline(paste("Оценка по", subjects[j], ": ")))
    grades[i, j] <- grade
  }
}

# Вывод данных об успеваемости
print("Данные об успеваемости:")
print(grades)

# Построение круговой диаграммы успеваемости для третьего студента
library(plotrix)  # Подключение библиотеки для построения круговых диаграмм

student_index <- 3  # Индекс третьего студента
grades_student <- grades[student_index, ]
pie(grades_student, labels = subjects, main = paste("Успеваемость", students[student_index]))

#задание 3

# Определяем функцию
f <- function(x) {
  (3*x^2 - 30*x + 74.417) / (x - 5)
}

# Определяем числитель функции
numerator <- function(x) {
  3*x^2 - 30*x + 74.417
}

# Находим корни числителя
numerator_roots <- polyroot(c(74.417, -30, 3))

# Определяем производную функции
f_prime <- function(x) {
  u <- 3*x^2 - 30*x + 74.417
  v <- x - 5
  u_prime <- 6*x - 30
  v_prime <- 1
  (u_prime * v - u * v_prime) / (v^2)
}

# Определяем производную числителя
numerator_prime <- function(x) {
  6*x - 30
}

# Найдем экстремумы функции
# Нам нужно найти корни производной
root_intervals <- function(f_prime, intervals) {
  roots <- c()
  for (interval in intervals) {
    try({
      root <- uniroot(f_prime, interval)$root
      roots <- c(roots, root)
    }, silent = TRUE)
  }
  return(roots)
}

# Интервалы для поиска корней производной
intervals <- list(c(-10, 4), c(6, 20))
extrema <- root_intervals(numerator_prime, intervals)

# Построение графиков
x_vals <- seq(0, 10, length.out = 1000)
y_vals <- f(x_vals)
y_prime_vals <- f_prime(x_vals)

# График функции
plot(x_vals, y_vals, type="l", col="blue", lwd=2, main="График функции f(x)", ylab="f(x)", xlab="x")
abline(h=0, col="red", lty=2)

# График производной функции
plot(x_vals, y_prime_vals, type="l", col="green", lwd=2, main="График производной f'(x)", ylab="f'(x)", xlab="x")
abline(h=0, col="red", lty=2)

# Выводим нули функции
cat("Нули функции (числитель):", numerator_roots, "\n")

# Выводим экстремальные точки (нули производной функции)
cat("Экстремальные точки (нули производной функции):", extrema, "\n")


#задание 4
f <- function(x) {
  x^3 - 18*x^2 + 106.25*x - 205.5
}


f_prime <- function(x) {
  3*x^2 - 36*x + 106.25
}


x <- seq(0, 30, length=100)
y <- seq(-200, 200, length=100)
z <- outer(x, y, function(x, y) f(x))
z_prime <- outer(x, y, function(x, y) f_prime(x))

#построение контуров функции f(x) и производной f'(x)
contour(x, y, z, levels=0, drawlabels=FALSE, xlab='X', ylab='f(x)', main='Contour of f(x)')
contour(x, y, z_prime, levels=0, drawlabels=FALSE, xlab='X', ylab="f'(x)", main='Contour of f\'(x)')



#построение 3D поверхности функции f(x) и производной f'(x)
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = 'lightblue', xlab = 'X', ylab = 'Y', zlab = 'f(x)', main = '3D Surface of f(x)')
persp(x, y, z_prime, theta = 30, phi = 30, expand = 0.5, col = 'lightblue', xlab = 'X', ylab = 'Y', zlab = "f'(x)", main = "3D Surface of f'(x)")
#задание 5
plot_elliptical_cylinder <- function() {
  # Ввод параметров пользователем
  a <- as.numeric(readline(prompt = "Введите значение для радиуса a: "))
  b <- as.numeric(readline(prompt = "Введите значение для радиуса b: "))
  height <- as.numeric(readline(prompt = "Введите значение для высоты: "))
  
  # Проверка на корректность введенных данных
  if (is.na(a) || is.na(b) || is.na(height)) {
    cat("Ошибка: Введите числовые значения для радиусов a и b, а также для высоты.\n")
    return(NULL)
  }
  
  # Создаем сетку координат для эллипса
  u <- seq(0, 2 * pi, length.out = 100)
  v <- seq(0, 2 * pi, length.out = 100)
  
  # Вычисляем координаты X, Y и Z для эллиптического цилиндра
  x <- outer(a * cos(u), b * cos(v))
  y <- outer(a * sin(u), b * cos(v))
  z <- outer(rep(height, length(u)), b * sin(v))
  
  # Построение 3D графика эллиптического цилиндра
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = 'lightblue', 
        xlab = 'X', ylab = 'Y', zlab = 'Z', main = '3D Surface of Elliptical Cylinder')
}

# Пример использования функции
plot_elliptical_cylinder()


#задание 6
plot_hyperbolic_cylinder <- function() {
  # Ввод параметров пользователем
  a <- as.numeric(readline(prompt = "Введите значение для параметра a: "))
  b <- as.numeric(readline(prompt = "Введите значение для параметра b: "))
  height <- as.numeric(readline(prompt = "Введите значение для высоты: "))
  
  # Проверка на корректность введенных данных
  if (is.na(a) || is.na(b) || is.na(height)) {
    cat("Ошибка: Введите числовые значения для параметров a, b и высоты.\n")
    return(NULL)
  }
  
  # Создаем сетку координат для гиперболы
  u <- seq(-pi, pi, length.out = 100)
  v <- seq(-pi, pi, length.out = 100)
  
  # Вычисляем координаты X, Y и Z для гиперболического цилиндра
  x <- outer(a * cosh(u), b * sinh(v))
  y <- outer(a * sinh(u), b * cosh(v))
  z <- outer(rep(height, length(u)), v)
  
  # Построение 3D графика гиперболического цилиндра
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = 'lightblue', 
        xlab = 'X', ylab = 'Y', zlab = 'Z', main = '3D Surface of Hyperbolic Cylinder')
}

# Пример использования функции
plot_hyperbolic_cylinder()

#задание 7
# Определяем функцию для построения гиперболоида
plot_hyperboloid <- function(a, b, c) {
  # Определяем сетку значений для x и y
  x <- seq(-2 * a, 2 * a, length.out = 100)
  y <- seq(-2 * b, 2 * b, length.out = 100)
  
  # Создаем массивы для хранения значений z
  z_pos <- matrix(NA, nrow = length(x), ncol = length(y))
  z_neg <- matrix(NA, nrow = length(x), ncol = length(y))
  
  # Вычисляем значения z для верхней и нижней частей гиперболоида
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      value <- (x[i]^2 / a^2 + y[j]^2 / b^2 - 1)
      if (value >= 0) {
        z_pos[i, j] <- sqrt(value * c^2)
        z_neg[i, j] <- -sqrt(value * c^2)
      }
    }
  }
  
  # Убираем NA значения и заменяем их на 0 для корректного отображения
  z_pos[is.na(z_pos)] <- 0
  z_neg[is.na(z_neg)] <- 0
  
  # Построение 3D графика гиперболоида
  persp(x, y, z_pos, theta = 30, phi = 30, expand = 0.5, col = 'lightblue', xlab = "X", ylab = "Y", zlab = "Z", main = "Однополостный гиперболоид (верхняя часть)")
  
  # Открытие нового окна графика для нижней части гиперболоида
  dev.new()
  persp(x, y, z_neg, theta = 30, phi = 30, expand = 0.5, col = 'lightblue', xlab = "X", ylab = "Y", zlab = "Z", main = "Однополостный гиперболоид (нижняя часть)")
}

# Ввод параметров пользователем
a <- as.numeric(readline(prompt = "Введите параметр a: "))
b <- as.numeric(readline(prompt = "Введите параметр b: "))
c <- as.numeric(readline(prompt = "Введите параметр c: "))

# Проверка на корректность введенных данных
if (is.na(a) || is.na(b) || is.na(c)) {
  cat("Ошибка: Введите числовые значения для параметров a, b и c.\n")
} else {
  # Построение гиперболоида с введенными параметрами
  plot_hyperboloid(a, b, c)
}
#задание 8
# Ввод параметров пользователем
R <- as.numeric(readline(prompt = "Введите значение для R: "))
r <- as.numeric(readline(prompt = "Введите значение для r: "))

# Проверка на корректность введенных данных
if (is.na(R) || is.na(r)) {
  cat("Ошибка: Введите числовые значения для параметров R и r.\n")
} else {
  # Создаем сетку координат для параметрического построения тора
  u <- seq(0, 2 * pi, length.out = 100)
  v <- seq(0, 2 * pi, length.out = 100)
  
  # Вычисляем координаты X, Y и Z для тора
  x <- outer(cos(u), (R + r * cos(v)))
  y <- outer(sin(u), (R + r * cos(v)))
  z <- outer(rep(1, length(u)), r * sin(v))
  
  # Построение 3D графика тора
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = 'lightblue', 
        xlab = 'X', ylab = 'Y', zlab = 'Z', main = '3D Surface of Torus')
}
