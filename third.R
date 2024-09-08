#часть 1

#задание 1

names <- c("Jane", "Michael", "Mary", "George")
ages <- c(8, 6, 28, 45)
gender <- c(0, 1, 0, 1)

#создание датафрейма - таблицы,  в которой могут быть разные
#типы данных (в отличие от матрицы)
df <- data.frame(Names = names, Ages = ages, Gender = gender)
print(df)

info <- list(name = names, age = ages, gender = gender)

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

cat(names(info), "\n")
cat(info$name, "\n")
cat(info$age, "\n")
cat(info$gender, "\n")
cat(info$drinks, "\n")

#задание 2

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

#install.packages("randomNames")
library(randomNames)

#задание 2

set.seed(1234) ##устанавливаем начальное значение для генерации случайных чисел
#чтобы у всех получались одинаковые результаты

#вектор из 100 испанских имен
names <- randomNames(100, which.names = "first", ethnicity = 4) 

#задание 3

#вектор со значениями возраста респондентов
ages <- sample(16:75, 100, replace = TRUE)
# replace = TRUE – с повторяющимися значениями 

#политические взгляды респондентов
views <- c("right", "left", "moderate", "indifferent")
polit <- sample(views, 100, replace = TRUE)

#создадим из трех полученных векторов датафрейм
df <- data.frame(names, ages, polit)

#задание 4

#cоздадим вектор с id для каждой строки
id <- 1:nrow(df)

#добавляем вектор в качестве нового столбца в датафрейм
df$id <- id

#задание 5

#install.packages("dplyr")
#dplyr – библиотека для удобной работы с датафреймами
library(dplyr) 

#usl - условие: выбираем людей старше 25 и младше 30
usl <- filter(df, ages >= 25 & ages <= 30)

#nrow - считает кол-во строк во фрейме
dolya <- (nrow(usl) / nrow(df))
print(dolya)

#задание 6

polit_views <- factor(df$polit)

#часть 3

#задание 1

#есть втроенный в car датафрейм Ornstein
#мы создаем Firms с данными  из него
library(car)
Firms <- Ornstein
print(Firms)

#задание 2

#количество наблюдений
print(nrow(Firms))


#количество переменных
print(ncol(Firms))


#имена переменных
print(colnames(Firms))

#задание 3

#строки, содержащие NA
complete <- Firms[rowSums(is.na(Firms)) == 1,]
print(complete)
#значит, в датафрейме Firms нет строк с пропущенными данными

#скорее всего нас спрашивали, есть ли в датафрейме строоки с нулями
#тогда поменяем нули на NA (датафрейм Firms_complete)
Firms <- replace(Firms, Firms == 0, NA)
print(Firms)
#и повторим действие (complete - датафрейм из строк матрицы, где есть NA)
complete <- Firms_complete[rowSums(is.na(Firms_complete)) == 1,]
print(complete)
#выведем кол-во строк в датафрейме complete
print(nrow(complete))

#задание 4

#отфильтруем наблюдения в таблице согласно критериям
#фирмы с активами от 10000 до 20000 (включительно)
krit1 <- (10000 <= Firms$assets) & (Firms$assets <= 20000)
#фирмы, число управляющих позиций, совместных с другими фирмами, которых не превышает 30
krit2 <- Firms$interlocks <= 30
#фирмы транспортного сектора (TRN) под руководством управляющих из Канады (CAN)
krit3 <- (Firms$sector == "TRN") & (Firms$nation == "CAN")

Firms_1 <- filter(Firms, krit1)
print(Firms_1)

Firms_2 <- filter(Firms, krit2)
print(Firms_2)

Firms_3 <- filter(Firms, krit3)
print(Firms_3)


#задание 5

#создадим переменную log_assets
Firms$log_assets <- log(Firms$assets)
print(Firms)

#задание 6

#задание 7

#задание 8

#часть 4

#задание 1


#install.packages(c("dplyr","readr", "stringr")) 
library(readr)
#читаем файл
df <- read_csv("C:/Users/Ramil/Desktop/time_series_covid19_confirmed_global.csv")
print(df)

#задание 2

#размерность таблицы
print(dim(df))

#имена столбцов
print(colnames(df))

#тип данных в столбцах
classes <- sapply(df, class)
#sapply применяет функцию class к элементам df
print(classes) #в первых двух столбцах character, в остальных - numeric

#задание 3

library(tidyr)

#объединяем колонки "Province/State" и "Country/Region" через запятую, создавая новую колонку "Location"
df2 <- unite(df, "Location", "Province/State", "Country/Region", sep = ",")

#задание 4

#na.rm = TRUE указывает R игнорировать пропущенные значения при суммировании
#если na.rm не указан или установлен в FALSE (что является значением по умолчанию),
#то любое NA в строке будет возвращать NA как результат
df2$"Сумма заболевших" <- rowSums(df2[, 4:ncol(df2)], na.rm = TRUE)
#суммируем с 5 по последний столбы
print(df2$"Сумма заболевших")

df2$"Среднее число заболевших" <- df2$"Сумма заболевших"/(ncol(df2) - 4 + 1)
print(df2$"Среднее число заболевших")


#1 здесь означает, что мы идем по строкам
df2$"Среднее отклонение" <- apply(df2[, 4:ncol(df2) - 1], 1, sd)
print(df2$"Среднее отклонение")

df2 <- df2[, -c(4:(ncol(df2) - 3))]
print(df2)

#задание 5

library(dplyr)

df_itog <- read_csv("C:/Users/Ramil/Desktop/time_series_covid19_confirmed_global.csv")

#объединяем Province/State и Country/Region в Location
df_itog <- unite(df, "Location", "Province/State", "Country/Region", sep = ",")
print(df_itog)

#минус значит, что удаляем столбцы
df_itog <- df_itog[, -c(2:3)]
print(df_itog)

#чтоб можно было использовать rownames()
df_itog <- as.data.frame(df_itog)

#заменяем все столбцы кроме 1 на нужный формат
for (i in 2:ncol(df_itog)) {
  colnames(df_itog)[i] <- as.Date(colnames(df_itog)[i], format="%m/%d/%y")
}

print(df_itog)

#строкам присваиваем значения из столбца Location
rownames(df_itog) <- df_itog$Location
#удаляем столбец Location
df_itog$Location <- NULL
#транспонируем датафрейм
df_itog <- t(df_itog)
print(df_itog)

#задание 6

#экспорт в формате .txt
write.table(df_itog, file = "rabota3.txt", row.names = F) 

#экспорт в формате .csv
write.csv(df_itog, file = "rabota3.csv") 

#экспорт в формате .xlsx
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("writexl")
}

#устанавливаем пакет openxlsx
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}

#загружаем пакет openxlsx
library(openxlsx)

#создаем поддиректорию, если ее не существует
if (!dir.exists("data_output")) {
  dir.create("data_output")
}

write.xlsx(df_itog, file = "data_output/rabota3.xlsx", rowNames = FALSE)