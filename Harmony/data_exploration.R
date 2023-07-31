# load in libraries 
library("dplyr") 
library("ggplot2") 
library("tidyverse") 
library("parsnip") 
library("reshape2") 
library("rsample") 
library("MLmetrics") 

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

# generate new data frame for country averages/proportions 
country <- country_count$economy 
country_temp <- filter(db, economy %in% country) 
income_countries <- filter(income_countries, Country.name %in% country)
country_avg <- summarize(country_temp, 
                         Average_education=mean(educ, na.rm=TRUE), 
                         .by=economy)  
country_temp_account <- filter(country_temp, account_fin==1) 
account_prop <- summarize(country_temp_account, 
                          Owns_account=n(), 
                          .by=economy)
account_prop <- mutate(account_prop, Owns_account_prop = Owns_account / country_count$n)
country_temp_saved <- filter(country_temp, saved==1) 
saved_prop <- summarize(country_temp_saved, 
                        Saved=n(), 
                        .by=economy)
saved_prop <- mutate(saved_prop, Saved_prop = Saved / country_count$n)
country_avg <- mutate(country_avg, 
                      Owns_account_prop = account_prop$Owns_account_prop, 
                      Saved_prop = saved_prop$Saved_prop, 
                      Income_level = income_countries$Income.group)

# perform linear regression analysis on correlation between education level and saved_prop 
head(country_avg) 

country_avg_corr <- select(country_avg, 
                           Average_education, Owns_account_prop, Saved_prop)
country_correlation <- cor(country_avg_corr) |> 
  melt() |> 
  as.data.frame() 
ggplot(country_correlation, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low="darkblue", mid="white", high="darkred", midpoint=0) + 
  theme_minimal() 

set.seed(123) 
data_reg_split <- initial_split(country_avg, prop=0.75) 
train_reg_data <- training(data_reg_split) 
test_reg_data <- testing(data_reg_split) 

linreg_fit <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression") |> 
  fit(Owns_account_prop ~ Average_education, data=train_reg_data) 
summary(linreg_fit$fit) 

saved_pred <- test_reg_data 
saved_pred$linReg <- predict(linreg_fit, test_reg_data)$.pred 
yardstick::mae(saved_pred, truth=Saved_prop, estimate=linReg) 
yardstick::rmse(saved_pred, truth=Saved_prop, estimate=linReg) 




# comp educ & saved_prop -> regression 



country_avg <- mutate(country_avg, 
                         Owns_account_proportion=count(db$account_fin==1), 
                         .by=economy)





  
head(country_avg) 







  
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



# db_grouped <- group_by(db, economy)


# plot income group distribution 
ggplot(income_countries, aes(x=Income.group)) + 
  geom_bar() + 
  labs(title = "Income group distribution") 






# dependent: numerical/financial literacy 
# use machine learning for correlation 

# RQ: To what extent does national income level influence improvement in financial literacy over time? 