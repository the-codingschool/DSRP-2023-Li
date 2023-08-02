library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(reshape2)
library(rsample)
library(parsnip)
#install.packages("ranger")
data <- readRDS("data/micro_world_139countries.RDS")
View(data)
summary(data)
new_data <- select(data, economy, economycode, regionwb, pop_adult, wpid_random, wgt, female, age, educ, inc_q,
                   emp_in, urbanicity_f2f, account, account_fin,
                   account_mob, fin2, fin11_1, fin11a, fin11b, fin11c, fin11d, fin11e, fin11f, fin11g,
                   fin11h)
data_plot <- select( data, educ, fin5, inc_q )

data_plot <- na.omit(data_plot)

data_plot_Cors <- data_plot |>
  cor() |>
  melt() |>
  as.data.frame()

ggplot(data_plot_Cors, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0)
data_plot_Cors
ggplot(data_plot, aes(x= inc_q , y= fin5)) +
  geom_violin() + labs(title = "Income vs. Internet Access")

noNAs <- filter(data, !is.na(inc_q), !is.na(educ), !is.na(fin5))

table1 <- table(noNAs$inc_q, noNAs$educ, noNAs$fin5)

table2 <- table()

count_inc <- count(noNAs, noNAs$inc_q)
count_inc

count_educ <- count(noNAs, noNAs$educ)
count_educ

count_fin5 <- count(noNAs, noNAs$fin5)
sum(count_fin5$n)
sum(count_educ$n)
sum(count_inc$n)

chisq.test(table1)
chisq.test(table1, noNAs$fin5)


chisq.test(noNAs$fin5, noNAs$educ, simulate.p.value = TRUE)


chisq.test(noNAs$fin5, noNAs$inc_q)

set.seed(71723)

noNAs_3 <- select(noNAs, fin5, educ, inc_q)

noNAs_factor <- mutate(noNAs_3, educ = as.factor(educ),
                      inc_q = as.factor(inc_q),
                      fin5 = as.factor(fin5))

class_split <- initial_split(noNAs_factor, prop = 0.75)

class_train <- training(class_split)
class_test <- testing(class_split)


forest_class_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(fin5 ~ ., data = class_train)

forest_class_fit$fit

install.packages("Metrics")
library(Metrics)
class_results <- class_test

class_results$forest_pred <- predict(forest_class_fit, class_test)$.pred_class

table(class_results$forest_pred)

View(class_results)
(class_results$fin5 == 1)

f1(class_results$fin5, class_results$forest_pred)

?as.factor

