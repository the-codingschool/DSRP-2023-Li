microworld <- readRDS("~/Documents/ResearchProject-Li/data/micro_world_139countries.RDS")
View(microworld)

library(dplyr)

## What is the relationship between different regions and account ownership

## Selecting columns related to research question
smallworld <- select(microworld, economy, account, account_mob, fin2, fin4, fin7, fin8, fin8b,
                     fin9, fin9a, fin10,fin10a, fin10b, regionwb)

## getting rid of NAs
Newsmallworld <- na.omit(smallworld)

## renaming columns related to research question
rename(Newsmallworld, country = "economy") 
rename(Newsmallworld, has_credit_card = "fin7") 
rename(Newsmallworld, has_debitcard = "fin2") 
rename(Newsmallworld, used_debitcard = "fin4")
rename(Newsmallworld, paid_creditbalace_infull = "fin8b")
rename(Newsmallworld, made_deposit = "fin9") 
rename(Newsmallworld, makes_deposit_2pluspermonth = "fin9a") 
rename(Newsmallworld, withdrew = "fin10") 
rename(Newsmallworld, withdrew_2plusamonth = "fin10a") 
rename(Newsmallworld, storemoneyinaccount = "fin10b") 

## Filtering by region
smallworldSouthAsia <- filter(Newsmallworld, Newsmallworld$regionwb == "South Asia")
smallworldEAsia_Pacific <- filter(Newsmallworld, Newsmallworld$regionwb  == "East Asia & Pacific (excluding high income)") 
smallworldEurope_CAsia <- filter(Newsmallworld, Newsmallworld$regionwb  == "Europe & Central Asia (excluding high income)") 
smallworldHighIncome <- filter(Newsmallworld, Newsmallworld$regionwb == "High income") 
smallworldLatin_Caribbean <- filter(Newsmallworld, Newsmallworld$regionwb  == "Latin America & Caribbean (excluding high income)") 
smallworldMiddleEast_NAfrica <- filter(Newsmallworld, Newsmallworld$regionwb  == "Middle East & North Africa (excluding high income)") 
smallworldSSAfrica <- filter(Newsmallworld, Newsmallworld$regionwb  == "Sub-Saharan Africa (excluding high income)") 

has_credit_card <- filter(Newsmallworld, )


library(ggplot2)

ggplot(data = smallworldSouthAsia, aes( x = economy , y= fin7)) +
  geom_bar(stat = "summary",
           fun = "mean", fill = "darkgreen" ) + 
  labs(x = "Country", y = "Owns Credit Card" , title = "What % of the South Asia Region owns a credit card")

ggplot(data = smallworldHighIncome, aes( x = economy , y= fin7)) +
  geom_bar(stat = "summary",
           fun = "mean", fill = "lightblue" ) +
  labs(x = "Country", y = "Owns Credit Card", title = "What % of the High Income Region owns a credit card")

ggplot(data = smallworldSSAfrica, aes( x = economy , y= fin7)) +
  geom_bar(stat = "summary",
           fun = "mean", fill = "red" ) +
  labs(x = "Country", y = "Owns Credit Card", title = "What % of the Sub-Saharan Africa Region owns a credit card")

ggplot(data = smallworldLatin_Caribbean, aes( x = economy , y= fin7)) +
  geom_bar(stat = "summary",
           fun = "mean", fill = "violet" ) +
  labs(x = "Country", y = "Owns Credit Card", title = "What % of the Latin America and Caribbean Region owns a credit card")
  
ggplot(data = smallworldMiddleEast_NAfrica, aes( x = economy , y= fin7)) +
  geom_bar(stat = "summary",
           fun = "mean", fill = "darkblue" ) +
  labs(x = "Country", y = "Owns Credit Card", title = "What % of the Middle East and North African Region owns a credit card")
  
ggplot(data = smallworldEurope_CAsia, aes( x = economy , y= fin7)) +
  geom_bar(stat = "summary",
           fun = "mean", fill = "grey" ) +
  labs(x = "Country", y = "Owns Credit Card", title = "What % of the Europe and Central Asian Region owns a credit card")

ggplot(data = smallworldEAsia_Pacific, aes( x = economy , y= fin7)) +
  geom_bar(stat = "summary",
           fun = "mean", fill = "orange" ) +
  labs(x = "Country", y = "Owns Credit Card", title = "What % of the East Asia and Pacific Region owns a credit card")
