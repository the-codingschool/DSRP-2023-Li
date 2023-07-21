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


