library(readr)
library(dplyr)
data <- readRDS("data/micro_world_139countries.RDS")
View(data)
summary(data)
new_data <- select(data, economy, economycode, regionwb, pop_adult, wpid_random, wgt, female, age, educ, inc_q,
                   emp_in, urbanicity_f2f, account, account_fin,
                   account_mob, fin2, fin11_1, fin11a, fin11b, fin11c, fin11d, fin11e, fin11f, fin11g,
                   fin11h)
data_clean <- na.omit(new_data)

