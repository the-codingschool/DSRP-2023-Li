# load in libraries 
library("dplyr") 
library("ggplot2") 
library("tidyverse") 

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
db <- select(db, economy, Income.group, educ, account_fin, saved)

# remove countries with too little data 
country_count <- count(db, economy) 
country_count <- full_join(country_count, income_countries, by=c("economy"="Country.name"))
db <- full_join(db, country_count, by=c("economy"="economy")) 
country_count <- filter(country_count, n!=1) 
db <- filter(db, n!=1) 

# generate new data frame for country averages 
country <- country_count$economy 
country_temp <- filter(db, economy %in% country) 
country_avg <- summarize(country_temp, 
                         Average_education=mean(educ, na.rm=TRUE), 
                         .by=economy)  
country_avg <- summarize(country_avg, 
                         Owns_account_proportion=(account_fin_count$n/country_count$n), 
                         .by=economy)
  
  





# perform machine learning analysis on averages data frame 


  
  country_total <- count(country_temp, economy) 
  
  
  account_fin_count <- filter(country_temp, account_fin==1) 
  account_fin_count <- count(account_fin_count, economy) 
  account_fin_prop <- account_fin_count$n / country_total$n 
  
  saved_count <- filter(country_temp, saved==1) 
  saved_count <- count(saved_count, economy) 
  saved_prop <- saved_count$n / country_total$n 
  
  
  
  
  country_avg <- rbind(country_avg, data.frame(c(country), c(country_count$Income.group), c(educ_avg), c(account_fin_prop), c(saved_prop)))
  
  
  # add_row 
  
  if x == 59 { 
      break 
    }
  x += 1 
  
}









country_temp <- filter(db, economy=="Afghanistan") 






# db_grouped <- group_by(db, economy)


# plot income group distribution 
ggplot(income_countries, aes(x=Income.group)) + 
  geom_bar() + 
  labs(title = "Income group distribution") 






# dependent: numerical/financial literacy 
# use machine learning for correlation 

# RQ: To what extent does national income level influence improvement in financial literacy over time? 