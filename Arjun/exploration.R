library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(reshape2)
library(rsample)
library(parsnip)
library(ranger)
library(corrplot)
data <- readRDS("data/micro_world_139countries.RDS")
View(data)
summary(data)
new_data <- select(data, economy, economycode, regionwb, pop_adult, wpid_random, wgt, female, age, educ, inc_q,
                   emp_in, urbanicity_f2f, account, account_fin,
                   account_mob, fin2, fin11_1, fin11a, fin11b, fin11c, fin11d, fin11e, fin11f, fin11g,
                   fin11h)
data_plot <- select( data, educ, internetaccess, inc_q )

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

noNAs <- filter(data, !is.na(inc_q), !is.na(educ), !is.na(internetaccess))

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


chisq.test(noNAs$internetaccess, noNAs$educ, simulate.p.value = TRUE)

chisq_educ <- chisq.test(noNAs$internetaccess, noNAs$educ)
corrplot(chisq_educ$residuals, is.cor = F)


chisq.test(noNAs$internetaccess, noNAs$inc_q)

chisq_inc <- chisq.test(noNAs$internetaccess, noNAs$inc_q)
corrplot(chisq_inc$residuals, is.cor = F)
set.seed(71723)

##1st ML Model Education, Income level, and Internet Access

noNAs_3 <- select(noNAs, internetaccess, educ, inc_q)

noNAs_factor <- mutate(noNAs_3, educ = as.factor(educ),
                      inc_q = as.factor(inc_q),
                      internetaccess = as.factor(internetaccess))

class_split <- initial_split(noNAs_factor, prop = 0.90)

class_train <- training(class_split)
class_test <- testing(class_split)


forest_class_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(internetaccess ~ ., data = class_train)

forest_class_fit$fit


library(Metrics)
class_results <- class_test

class_results$forest_pred <- predict(forest_class_fit, class_test)$.pred_class

table(class_results$forest_pred)

View(class_results)
(class_results$internetaccess == 1)

f1_score <- f1(class_results$internetaccess, class_results$forest_pred)



#2nd ML Model Education and Internet Access
noNAs_2A <- select(noNAs, internetaccess, educ)

noNAs_factor2A <- mutate(noNAs_2A, educ = as.factor(educ),
                         internetaccess = as.factor(internetaccess))

class_split2A <- initial_split(noNAs_factor2A, prop = 0.90)

class_train2A <- training(class_split2A)
class_test2A <- testing(class_split2A)


forest_class_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(internetaccess ~ ., data = class_train2A)

forest_class_fit$fit


library(Metrics)
class_results2A <- class_test2A

class_results2A$forest_pred <- predict(forest_class_fit, class_test2A)$.pred_class

table(class_results2A$forest_pred)

View(class_results2A)
(class_results$internetaccess == 1)

f2_score <- f1(class_results2A$internetaccess, class_results2A$forest_pred)

#3rd ML Model Education and Internet Access
noNAs_3A <- select(noNAs, internetaccess, inc_q)

noNAs_factor3A <- mutate(noNAs_3A, educ = as.factor(inc_q),
                         internetaccess = as.factor(internetaccess))

class_split3A <- initial_split(noNAs_factor3A, prop = 0.90)

class_train3A <- training(class_split3A)
class_test3A <- testing(class_split3A)


forest_class_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(internetaccess ~ ., data = class_train3A)

forest_class_fit$fit


library(Metrics)
class_results3A <- class_test3A

class_results3A$forest_pred <- predict(forest_class_fit, class_test3A)$.pred_class

table(class_results3A$forest_pred)

View(class_results3A)
(class_results$internetaccess == 1)

f3_score <- f1(class_results3A$internetaccess, class_results3A$forest_pred)


ggplot(data = iris, aes(y= Sepal.Length, x = Species)) +
  geom_bar(stat = "summary", fun = "mean")

Score <- merge(f1_score, f2_score, f3_score)
