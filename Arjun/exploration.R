library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(reshape2)
library(rsample)
library(parsnip)
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
