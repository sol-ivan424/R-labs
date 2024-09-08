#install.packages("readxl")
#install.packages("dplyr")

library(xlsx)
library(dplyr)

excel_file <- "itog.xlsx"

df <- read.xlsx(excel_file, sheetIndex = 1)

df <- as.data.frame(df)

print(str(df))
#задание 1
#меняем дату на нужный формат
df$Дата.замера <- as.Date(df$Дата.замера, format = "%Y-%m-%d")
df$дата.замера <- as.Date(df$дата.замера, format = "%Y-%m-%d")
#выводим типы данных
print(str(df))

print(colnames(df))
#задание 2
#удаляем строки с NULL
df <- df[rowSums(df == "NULL", na.rm = TRUE) == 0, ]
#задание 3
df <- df %>%
  mutate(Туст..K = as.numeric(Туст..С) + 273.15) %>%
  select(-Туст..С)
#задание 4
df$ID <- as.ordered(df$ID)
df$Куст <- as.ordered(df$Куст)
df$Группа <- as.ordered(df$Группа)

print(str(df))
#делаем столбцы 4-6 числовыми
df <- df %>%
  mutate_at(4:6, as.numeric)
#задание 5
#добавляем новые столбцы
df <- df %>%
  mutate(
    газ_конденсат = газ.м3.сут / конд.т.м3.сут,
    газ_вода = газ.м3.сут / вода.м3.сут,
    вода_конденсат = вода.м3.сут / конд.т.м3.сут
  )
#задание 6
df_2018 <- df %>%
  filter(year(Дата.замера) == 2018 | year(дата.замера) == 2018)
#задание 7
df_7 <- df %>%
  filter(ID <= 111)
#задание 8
df_8 <- df %>%
  group_by(ID) %>%
  filter(вода.м3.сут <= 2) %>%
  distinct(ID)
#задание 9
df_9 <- df %>%
  group_by(ID) %>%
  filter(газ.м3.сут + конд.т.м3.сут + вода.м3.сут >= 1000) %>%
  distinct(ID)

IDs <- df %>%
  distinct(ID)

print(nrow(IDs))
print(nrow(df_9))
#видим, что условию удовлетворяют все скважины
#задане 10
df_10 <- df_2018 %>%
  group_by(Группа) %>%
  summarise(max_gas_production = max(газ.м3.сут)) %>%
  filter(max_gas_production == max(max_gas_production)) %>%
  select(Группа)
#задание 11
df_11 <- df_2018 %>%
  group_by(Куст) %>%
  summarise(max_water_production = max(вода.м3.сут)) %>%
  filter(max_water_production == max(max_water_production)) %>%
  select(Куст)
#задание 12
df_12 <- df %>%
  group_by(Куст) %>%
  summarise(Среднее_отношение = mean(газ_вода, na.rm = TRUE)) %>%
  filter(Среднее_отношение == max(Среднее_отношение)) %>%
  select(Куст)

print(df_12)
