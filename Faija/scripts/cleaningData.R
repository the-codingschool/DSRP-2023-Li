library(readr)
data <- read_csv("data/DatabankWide Dataset - Data.csv")
View(data)

library(dplyr)

datasm <- select(data, Region, Year, `Country name`, 
                 `Paid off all credit card balances in full by their due date (% who used a credit card, age 15+)`,
                `Owns a debit or credit card (% age 15+)`, `Used a debit or credit card (% age 15+)`,
             `Made a deposit (% with a financial institution account, age 15+)`, `Country name`, `Income group`, 
            `Deposited money into a financial institution account 2 or more times a month (% age 15+)`,
            `Deposited money into a financial institution account 2 or more times a month (% who had deposited money, age 15+)`,
           `Withdrew money from a financial institution account 2 or more times a month (% age 15+)`,
            `Withdrew money from a financial institution account 2 or more times a month (% who had withdrawn money, age 15+)`,
            `Store money using a financial institution (% age 15+)`,`Store money using a financial institution (% with a financial institution account, age 15+)`) 
 
noNAs <- na.omit(datasm)

rename(datasm, Country = 'Country name')
rename(datasm, Owns_debit_or_credit_card = 'Owns a debit or credit card (% age 15+)')
rename(datasm, Used_debit_or_creditcard =  'Used a debit or credit card (% age 15+)')
rename(datasm, made_a_deposit = 'Made a deposit (% with a financial institution account, age 15+)')


library(ggplot2)

ggplot(data = datasm, aes( x = Country , y= Owns_debit_or_credit_card)) +
  geom_bar(stat = "summary",
           fun = "mean") 
labs( x = Country, y= Owns Debit or Credit card , 
      title = Percentage of countries with debit or credit cards)

## new data

microworld <- readRDS("~/Documents/ResearchProject-Li/data/micro_world_139countries.RDS")
View(microworld)

smallworld <- select(microworld, economy, account, account_mob, fin2, fin4, fin7, fin8, fin8b,
                     fin9, fin9a, fin10,fin10a, fin10b)

NewSmallWorld <- na.omit(smallworld)

rename(smallworld, country = "economy") 
rename(smallworld, has_credit_card = "fin7") 
rename(smallworld, has_debitcard = "fin2") 
rename(smallworld, used_debitcard = "fin4")
rename(smallworld, paid_creditbalace_infull = "fin8b")
rename(smallworld, made_deposit = "fin9") 
rename(smallworld, makes_deposit_2pluspermonth = "fin9a") 
rename(smallworld, withdrew = "fin10") 
rename(smallworld, withdrew_2plusamonth = "fin10a") 
rename(smallworld, storemoneyinaccount = "fin10b") 


smallworldUS <- filter(smallworld, smallworld$economy  == "United States") 


library(ggplot2)

ggplot(data = smallworldUS, aes( x = economy , y= fin7)) +
  geom_bar(stat = "summary",
           fun = "mean") 

 ggplot(data = microworld, aes(x = pop_adult)) +
   geom_boxplot()

 ggplot(data = microworld, aes(x = economy, y = pop_adult)) +
   geom_violin()
 
 
 
 