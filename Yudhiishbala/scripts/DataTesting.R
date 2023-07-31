library(dplyr)
dataframe <- read.csv("data/DatabankWide Dataset - Data.csv")
#View(dataframe)

micro139 <- readRDS("data/micro_world_139countries.RDS")
micro139$regionwb[micro139$regionwb == ""] <- "High income"
micro139$economy[micro139$economy == "T\xfcrkiye"] <- "Turkey"

df <- select(micro139, -economycode)

#df <- filter(df, is.na(account_mob))
#unique(df$economy)
df$regionwb[df$regionwb == "East Asia & Pacific (excluding high income)"] <- "East Asia & Pacific"
df$regionwb[df$regionwb == "Europe & Central Asia (excluding high income)"] <- "Europe & Central Asia"
df$regionwb[df$regionwb == "High income"] <- "High Income"
df$regionwb[df$regionwb == "Latin America & Caribbean (excluding high income)"] <- "Latin America & Caribbean"
df$regionwb[df$regionwb == "Middle East & North Africa (excluding high income)"] <- "Middle East & North Africa"
df$regionwb[df$regionwb == "Sub-Saharan Africa (excluding high income)"] <- "Sub-Saharan Africa"
df <- df %>% select("economy":"account_mob", "fin11a":"fin11h", "fin13_1a":"fin13_1f")

# da <- select(df, regionwb)
# library(ggplot2)
# ggplot(da, aes(x = regionwb)) +
#   geom_bar() +
#   theme_wsj()


find11a <- (count(df, account_fin == 0, fin11a == 1))[TRUE][TRUE]$n[5] * 1
find11b <- (count(df, account_fin == 0, fin11b == 1))[TRUE][TRUE]$n[5] * 1
find11c <- (count(df, account_fin == 0, fin11c == 1))[TRUE][TRUE]$n[5] * 1
find11d <- (count(df, account_fin == 0, fin11d == 1))[TRUE][TRUE]$n[5] * 1
find11e <- (count(df, account_fin == 0, fin11e == 1))[TRUE][TRUE]$n[5] * 1
find11f <- (count(df, account_fin == 0, fin11f == 1))[TRUE][TRUE]$n[5] * 1
find11g <- (count(df, account_fin == 0, fin11g == 1))[TRUE][TRUE]$n[5] * 1
find11h <- (count(df, account_fin == 0, fin11h == 1))[TRUE][TRUE]$n[5] * 1
data <- data.frame(column = c("fin11a","fin11b","fin11c","fin11d","fin11e","fin11f","fin11g","fin11h"),
                   count = c(find11a,find11b,find11c,find11d,find11e,find11f,find11g,find11h))

library(ggplot2)
ggplot(data,aes(column,count))+
  geom_bar(stat="identity")

result1 <- chisq.test(df$account_fin == 0, df$fin11a == 1, correct=FALSE)
result1$residuals
result1 <- result1$p.value


result2 <- chisq.test(df$account_fin == 0, df$fin11b == 1, correct=FALSE)
result2$residuals
result2 <- result2$p.value


result3 <- chisq.test(df$account_fin == 0, df$fin11c == 1, correct=FALSE)
result3$residuals
result3 <- result3$p.value


result4 <- chisq.test(df$account_fin == 0, df$fin11d == 1, correct=FALSE)
result4$residuals
result4 <- result4$p.value


result5 <- chisq.test(df$account_fin == 0, df$fin11e == 1, correct=FALSE)
result5$residuals
result5 <- result5$p.value


result6 <- chisq.test(df$account_fin == 0, df$fin11f == 1, correct=FALSE)
result6$residuals
result6 <- result6$p.value


result7 <- chisq.test(df$account_fin == 0, df$fin11g == 1, correct=FALSE)
result7$residuals
result7 <- result7$p.value


result8 <- chisq.test(df$account_fin == 0, df$fin11h == 1, correct=FALSE)
result8$residuals
result8 <- result8$p.value


df <- filter(df, regionwb == "Sub-Saharan Africa")

find13a <- (count(df, account_mob == 0, fin13_1a == 1))[TRUE][TRUE]$n[5] * 1
find13b <- (count(df, account_mob == 0, fin13_1b == 1))[TRUE][TRUE]$n[5] * 1
find13c <- (count(df, account_mob== 0, fin13_1c == 1))[TRUE][TRUE]$n[5] * 1
find13d <- (count(df, account_mob == 0, fin13_1d == 1))[TRUE][TRUE]$n[5] * 1
find13e <- (count(df, account_mob == 0, fin13_1e == 1))[TRUE][TRUE]$n[5] * 1
find13f <- (count(df, account_mob == 0, fin13_1f == 1))[TRUE][TRUE]$n[5] * 1
data <- data.frame(column = c("fin13_1a","fin13_1b","fin13_1c","fin13_1d","fin13_1e","fin13_1f"),
                   count = c(find13a,find13b,find13c,find13d,find13e,find13f))

library(ggplot2)
ggplot(data,aes(column,count))+
  geom_bar(stat="identity")

result1a <- chisq.test(df$account_mob == 0, df$fin13_1a == 1, correct=FALSE)
result1a$residuals
result_1 <- result1a$p.value
 
result2a <- chisq.test(df$account_mob == 0, df$fin13_1b == 1, correct=FALSE)
result2a$residuals
result_2 <- result2a$p.value
 
result3a <- chisq.test(df$account_mob == 0, df$fin13_1c == 1, correct=FALSE)
result3a$residuals
result_3 <- result3a$p.value

result4a <- chisq.test(df$account_mob == 0, df$fin13_1d == 1, correct=FALSE)
result4a$residuals
result_4 <- result4a$p.value

result5a <- chisq.test(df$account_mob == 0, df$fin13_1e == 1, correct=FALSE)
result5a$residuals
result_5 <- result5a$p.value

result6a <- chisq.test(df$account_mob == 0, df$fin13_1f == 1, correct=FALSE)
result6a$residuals
result_6 <- result6a$p.value

