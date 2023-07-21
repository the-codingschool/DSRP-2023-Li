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
 
datasm

rename(data, Country = 'Country name')



filter(datasm, is.na("Paid off all credit card balances in full by their due date (% who used a credit card, age 15+)"))
filter(datasm, is.na("Used a debit or credit card (% age 15+)"))

noNAs <- na.omit(data)
noNAs

library(ggplot2)


