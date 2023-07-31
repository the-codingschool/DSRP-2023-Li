microworld <- readRDS("~/Documents/ResearchProject-Li/data/micro_world_139countries.RDS")
View(microworld)
library(dplyr)
install.packages("tidymodels")
library(parsnip)
library(rsample)
library(yardstick)
install.packages("reshape2")
library(reshape2)
library(ggplot2)
install.packages("ranger")
install.packages("xgboost")

head(microworld)
noNAs <- na.omit(microworld)
noNAs <- filter(microworld, !is.na(microworld$fin7), !is.na(microworld$regionwb))
noNAs
replacewithmeans <- mutate(microworld,
                           fin7 = ifelse(is.na(fin7),
                                         mean(fin7),fin7))

intregion2 <- mutate(microworld, regionwb = as.integer(as.factor(regionwb)))

intregion2

microworldnumeric <- mutate(, regionwb = as.integer(regionwb)) |>
  
                    
microcleandata <- select(.data = microworld, regionwb, fin7, fin2, account_fin, account_mob,account, fin8,fin8a, fin4, fin4a) 


microworldCors2 <- cor(microworldnumeric) |>
  cor() |>
  melt() |>
  as.data.frame()
microworldCors2


ggplot(microworldCors, aes(x= Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "lightblue", mid = "pink", high = "violet", midpoint = 0) +
  theme_minimal()

set.seed(479)
data_class_split <- initial_split(factorfinseven, prop = 0.75)
train_class_data <- training(data_class_split)
test_class_data <- testing(data_class_split) 

rand_forest_fit2 <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(microworld$fin7 ~., data = train_class_data)

rand_forest_fit2$fit

boost_tree_fit2 <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(fin7 ~., data = train_class_data)


boost_tree_fit2$fit






