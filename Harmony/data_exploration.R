# load in libraries 
library("dplyr") 
library("ggplot2") 

# read in datasets 
db_large <- read.csv("../data/DatabankWide Dataset - Data.csv") 
db_small <- readRDS("../data/micro_world_139countries.RDS")

# select the countries from db_large in desired income groups 
income_countries <- filter(db_large, Income.group=="Low income" | Income.group=="High income") 
income_countries <- select(income_countries, Country.name, Income.group)
income_countries <- distinct(income_countries, Country.name, Income.group)

# merge income_countries with db_small 
db <- full_join(db_small, income_countries, by=c("economy"="Country.name"))
db <- filter(db, !is.na(Income.group))

# select relevant variables 
db <- select(db, economy, Income.group, educ, account_fin, fin8b, fin10_1d, fin10b, fin11_1, 
             fin16, fin17a, fin22a, fin24a, fin24b, fin44a, fin44b, fin44c, fin44d, fin45, 
             fin45_1, saved)

ggplot(db, aes(x=Income.group)) + 
  geom_bar() 







# dependent: numerical/financial literacy 
# use machine learning for correlation 

# RQ: To what extent does national income level influence improvement in financial literacy over time? 