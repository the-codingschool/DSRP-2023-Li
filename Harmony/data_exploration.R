# load in libraries 
library("dplyr") 

# read in CSV 
db_large <- read.csv("../data/DatabankWide Dataset - Data.csv")

# filter needed columns 
db_small <- filter(db_large, Income.group=="Low income" | Income.group=="Medium income" | Income.group=="High income") 
db_small <- select(db_small, c())


# group by income level 
# dependent: numerical/financial literacy 
# use machine learning for correlation 

# RQ: To what extent does national income level influence improvement in financial literacy over time? 