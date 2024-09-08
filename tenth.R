#############часть 1##############
#############задание 1##############

library(ggplot2)
library(dplyr)
df <- read.csv("https://raw.githubusercontent.com/allatambov/R-programming-3/master/seminars/sem8-09-02/demography.csv", encoding = "UTF-8")

#############задание 2##############
df <- df %>% mutate(young_share = young_total/popul_total * 100,
                    trud_share = wa_total/popul_total * 100,
                    old_share = ret_total/popul_total * 100)


#############задание 3##############
ggplot(data = df, aes(x = trud_share)) + 
  geom_histogram(fill = "tomato", color = "black", bins = 10) +
  labs(title = "Working age", x = "People in working age, %") +
  geom_vline(xintercept = median(df$trud_share), color = "blue", 
             lwd = 1, lty = 2) + geom_rug()

#############задание 4##############
ggplot(data = df, 
       aes(x = trud_share, group = region, fill = region)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("yellow", "green"))

#############задание 5##############
ggplot(data = df, aes(x = young_share, y= old_share)) +
  geom_point(color = "brown", shape = 8, size = 4) + theme_bw()

#############задание 6##############
df <- df %>% mutate(female = young_female + wa_female + ret_female,
                    male = young_male + wa_male + ret_male) %>% 
  mutate(fe_share = female / popul_total * 100,
         ma_share = male / popul_total * 100) %>%
  mutate(Male = factor(ifelse(ma_share > fe_share, 1, 0)))

#############задание 7##############
ggplot(data = df, aes(x = young_share, y= old_share)) +
  geom_point(aes(size = ma_share, color = Male))


ggplot(data = df, aes(x = region)) + geom_bar(fill = "purple", color = "black") + 
  scale_y_continuous(breaks = seq(0, 30, by = 5)) # больше делений по оси y


#############часть 2##############

#############задание 1##############
ggplot(data = mtcars, aes(x = hp, y = wt)) +
  geom_point(aes(size = cyl, color = as.factor(am))) +
  labs(title = "Характеристики автомобилей", 
       x = "Число лошадиных сил", 
       y = "Вес", 
       color = "Коробка передач", 
       size = "Число цилиндров") +
  scale_color_manual(values = c("green", "red"),                    
                     labels = c("Автомат", "Механика"))

#############задание 2##############
ggplot(data = mtcars, aes(x = hp)) + 
  geom_histogram(fill = "brown", 
                 color = "black", 
                 bins = 6) +  
  labs(title = "Gross horsepower", 
       x = "Horsepower", 
       y = "count") + 
  theme_bw() + 
  facet_grid(~am, 
             labeller = labeller(am = c("0" = "Automatic", 
                                        "1" = "Mechanic")))

#############задание 3##############
ggplot(data = datasets::sleep, 
       aes(x = "", y = extra, 
           group = group, fill = group)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("hotpink", "darkviolet")) +
  labs(title = "Student's Sleep", x = "Groups", y = "Extra time (hours)") +
  theme(plot.title = element_text(hjust = 0.5))


#############часть 3##############