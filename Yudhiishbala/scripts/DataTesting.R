library(dplyr)

micro139 <- readRDS("data/micro_world_139countries.RDS")
micro139$regionwb[micro139$regionwb == ""] <- "High income"
micro139$regionwb[micro139$regionwb == "East Asia & Pacific (excluding high income)"] <- "East Asia & Pacific"
micro139$regionwb[micro139$regionwb == "Europe & Central Asia (excluding high income)"] <- "Europe & Central Asia"
micro139$regionwb[micro139$regionwb == "High income"] <- "High Income"
micro139$regionwb[micro139$regionwb == "Latin America & Caribbean (excluding high income)"] <- "Latin America & Caribbean"
micro139$regionwb[micro139$regionwb == "Middle East & North Africa (excluding high income)"] <- "Middle East & North Africa"
micro139$regionwb[micro139$regionwb == "Sub-Saharan Africa (excluding high income)"] <- "Sub-Saharan Africa"

micro139$economy[micro139$economy == "C\xf4te d'Ivoire"] <- "Ivory Coast"
micro139$economy[micro139$economy == "Congo, Dem. Rep."] <- "Democratic Republic of the Congo"
micro139$economy[micro139$economy == "Congo, Rep."] <- "Republic of Congo"
micro139$economy[micro139$economy == "Czechia"] <- "Czech Republic"
micro139$economy[micro139$economy == "Egypt, Arab Rep."] <- "Egypt"
#micro139$economy[micro139f$economy == "Eswatini"] <- ""
micro139$economy[micro139$economy == "Gambia, The"] <- "Gambia"
micro139$economy[micro139$economy == "Hong Kong SAR, China"] <- "China"
micro139$economy[micro139$economy == "Iran, Islamic Rep."] <- "Iran"
micro139$economy[micro139$economy == "Korea, Rep."] <- "South Korea"
micro139$economy[micro139$economy == "Kyrgyz Republic"] <- "Kyrgyzstan"
micro139$economy[micro139$economy == "Lao PDR"] <- "Laos"
micro139$economy[micro139$economy == "Russian Federation"] <- "Russia"
micro139$economy[micro139$economy == "Slovak Republic"] <- "Slovakia"
micro139$economy[micro139$economy == "T\xfcrkiye"] <- "Turkey"
micro139$economy[micro139$economy == "Taiwan, China"] <- "Taiwan"
micro139$economy[micro139$economy == "United Kingdom"] <- "UK"
micro139$economy[micro139$economy == "United States"] <- "USA"
micro139$economy[micro139$economy == "Venezuela, RB"] <- "Venezuela"
#micro139$economy[micro139$economy == "West Bank and Gaza"] <- ""
micro139$economy[micro139$economy == "Yemen, Rep."] <- "Yemen"

df <- select(micro139, -economycode)
df <- df %>% select("economy":"account_mob","internetaccess","fin11a":"fin11h", "fin13_1a":"fin13_1f","mobileowner")

map <- map_data("world")
dfArrCount <- count(df,economy)
map_data<-left_join(map, dfArrCount, by=c("region"="economy"))
ggplot(map_data, aes(x=long, y= lat, group=group, fill=n, map_id = region))+
  geom_map(map = map)+
  scale_fill_viridis(option = "G", begin = 0.3, end = 0.85)+
  labs(x = "Longitude", y = "Latitude", title = "Number of Respondents per Country")+
  guides(fill = guide_legend(title = "Responses")) + 
  scale_x_continuous(breaks = seq(-180,180,30))+
  scale_y_continuous(breaks = seq(-90,90,30))+
  theme_wsj()+
  theme(plot.title = element_text(size = 26),
        axis.title = element_text(size = 15),
        axis.text = element_text(vjust = 0.9, size = 10, color="black"),
        legend.text = element_text(size = 15))
  

agedata <- filter(df, !is.na(df$age))
agedata <- count(agedata, age)
color_breaks <- c(16,18,35,50,65,80)
colors <- c("#03092b","#4d127a","#a3175b","#a3171c","#db5112","#db9512")
ggplot(agedata, aes(x = age,y = n)) +
  geom_point(aes(color = age), size = 5)+
  scale_color_gradientn(limits  = range(agedata$age),
                        colors = colors[c(1, seq_along(colors), length(colors))],
                        values  = c(0, scales::rescale(color_breaks, from = range(agedata$age)), 1))+
  scale_x_continuous(breaks = seq(15,100,5))+
  labs(title = "Respondents by Age", x = "Age",y = "Count")+
  guides(color = guide_legend(title = "Age"))+
  theme_wsj()+
  theme(plot.title = element_text(size = 26),
        axis.title = element_text(size = 15),
        axis.text = element_text(vjust = 0.9, size = 10, color="black"),
        legend.text = element_text(size = 15))
  

find11a <- (count(df, account_fin == 0, fin11a == 1))[TRUE][TRUE]$n[5] * 1
find11b <- (count(df, account_fin == 0, fin11b == 1))[TRUE][TRUE]$n[5] * 1
find11c <- (count(df, account_fin == 0, fin11c == 1))[TRUE][TRUE]$n[5] * 1
find11d <- (count(df, account_fin == 0, fin11d == 1))[TRUE][TRUE]$n[5] * 1
find11e <- (count(df, account_fin == 0, fin11e == 1))[TRUE][TRUE]$n[5] * 1
find11f <- (count(df, account_fin == 0, fin11f == 1))[TRUE][TRUE]$n[5] * 1
find11g <- (count(df, account_fin == 0, fin11g == 1))[TRUE][TRUE]$n[5] * 1
find11h <- (count(df, account_fin == 0, fin11h == 1))[TRUE][TRUE]$n[5] * 1

dataf <- data.frame(column = c("a. Too Far","b. Too Expensive","c. Lack Documentation","d. Lack Trust",
                              "e. Religious Reasons","f. Lack Money","g. Family Member Already Has One",
                              "h. No Need For Financial Services"),
                   count = c(find11a,find11b,find11c,find11d,find11e,find11f,find11g,find11h))

library(ggplot2)
ggplot(data = dataf,aes(x = column, y = count, fill = column))+
  geom_bar(stat="identity")+
  ggthemes::theme_wsj()+
  scale_fill_manual(values = c("#8a3b3b", "#9e4d16", "#b0aa56", "#4c8c46", "#468c7e", "#46568c", "#69468c", "#942987"))+
  theme(plot.title = element_text(size = 26),
        axis.title = element_text(size = 15),
        axis.text = element_text(vjust = 0.9, size = 10, color="black"),
        legend.position = "none") +
  labs(x = "Reason",
       y = "Number of People",
       title = "Reasons For Lacking a Financial Account")+ 
  scale_x_discrete(labels = function(x)stringr::str_wrap(x, width = 15))

#fin11a Chi-Square Test
result1 <- chisq.test(df$account_fin == 0, df$fin11a == 1, correct=FALSE)
result1$residuals
result1 <- result1$p.value

#fin11b Chi-Square Test
result2 <- chisq.test(df$account_fin == 0, df$fin11b == 1, correct=FALSE)
result2$residuals
result2 <- result2$p.value

#fin11c Chi-Square Test
result3 <- chisq.test(df$account_fin == 0, df$fin11c == 1, correct=FALSE)
result3$residuals
result3 <- result3$p.value

#fin11d Chi-Square Test
result4 <- chisq.test(df$account_fin == 0, df$fin11d == 1, correct=FALSE)
result4$residuals
result4 <- result4$p.value

#fin11e Chi-Square Test
result5 <- chisq.test(df$account_fin == 0, df$fin11e == 1, correct=FALSE)
result5$residuals
result5 <- result5$p.value

#fin11f Chi-Square Test
result6 <- chisq.test(df$account_fin == 0, df$fin11f == 1, correct=FALSE)
result6$residuals
result6 <- result6$p.value

#fin11g Chi-Square Test
result7 <- chisq.test(df$account_fin == 0, df$fin11g == 1, correct=FALSE)
result7$residuals
result7 <- result7$p.value

#fin11h Chi-Square Test
result8 <- chisq.test(df$account_fin == 0, df$fin11h == 1, correct=FALSE)
result8$residuals
result8 <- result8$p.value



dataframe <- filter(df, regionwb == "Sub-Saharan Africa")

find13a <- (count(dataframe, account_mob == 0, fin13_1a == 1))[TRUE][TRUE]$n[5] * 1
find13b <- (count(dataframe, account_mob == 0, fin13_1b == 1))[TRUE][TRUE]$n[5] * 1
find13c <- (count(dataframe, account_mob== 0, fin13_1c == 1))[TRUE][TRUE]$n[5] * 1
find13d <- (count(dataframe, account_mob == 0, fin13_1d == 1))[TRUE][TRUE]$n[5] * 1
find13e <- (count(dataframe, account_mob == 0, fin13_1e == 1))[TRUE][TRUE]$n[5] * 1
find13f <- (count(dataframe, account_mob == 0, fin13_1f == 1))[TRUE][TRUE]$n[5] * 1
find13fb <- (count(dataframe, account_mob == 0, fin13_1f == 1 & mobileowner != 1))[TRUE][TRUE]$n[5] * 1

#Unmodified fin13_1f column
datas <- data.frame(column = c("a. Too Far","b. Too Expensive","c. Lack Documentation","d. Lack of Money","e. Use Agent","f. No Mobile Phone"),
                   count = c(find13a,find13b,find13c,find13d,find13e,find13f))

library(ggplot2)
ggplot(data = datas,aes(x = column, y = count, fill = column))+
  geom_bar(stat="identity")+
  ggthemes::theme_wsj()+
  scale_fill_manual(values = c("#4c8c46", "#468c7e", "#46568c", "#364075", "#69468c", "#942987"))+
  theme(plot.title = element_text(size = 26),
        axis.title = element_text(size = 15),
        axis.text = element_text(vjust = 0.9, size = 10, color="black"),
        legend.position = "none") +
  labs(x = "Reason",
       y = "Number of People",
       title = "Reasons For Lacking a Mobile Money Account")+ 
  scale_x_discrete(labels = function(x)stringr::str_wrap(x, width = 15))

#Filtered fin13_1f column
datab <- data.frame(column = c("a. Too Far","b. Too Expensive","c. Lack Documentation","d. Lack of Money","e. Use Agent","f. No Mobile Phone"),
                    count = c(find13a,find13b,find13c,find13d,find13e,find13fb))

library(ggplot2)
ggplot(data = datab,aes(x = column, y = count, fill = column))+
  geom_bar(stat="identity")+
  ggthemes::theme_wsj()+
  scale_fill_manual(values = c("#4c8c46", "#468c7e", "#46568c", "#364075", "#69468c", "#942987"))+
  theme(plot.title = element_text(size = 26),
        axis.title = element_text(size = 15),
        axis.text = element_text(vjust = 0.9, size = 10, color="black"),
        legend.position = "none",
        plot.subtitle = element_text(size = 14)) +
  labs(x = "Reason",
       y = "Number of People",
       title = "Reasons For Lacking a Mobile Money Account",
       subtitle = "Excludes about 2000 contradicting responses (Lacks Mobile Phone but listed as having a mobile phone)")+ 
  scale_x_discrete(labels = function(x)stringr::str_wrap(x, width = 15))

#fin13_1a Chi-Square Test
result_1 <- chisq.test(df$account_mob == 0, df$fin13_1a == 1, correct=FALSE)
result_1$residuals
result_1 <- result_1$p.value

#fin13_1b Chi-Square Test 
result_2 <- chisq.test(df$account_mob == 0, df$fin13_1b == 1, correct=FALSE)
result_2$residuals
result_2 <- result_2$p.value

#fin13_1c Chi-Square Test
result_3 <- chisq.test(df$account_mob == 0, df$fin13_1c == 1, correct=FALSE)
result_3$residuals
result_3 <- result_3$p.value

#fin13_1d Chi-Square Test
result_4 <- chisq.test(df$account_mob == 0, df$fin13_1d == 1, correct=FALSE)
result_4$residuals
result_4 <- result_4$p.value

#fin13_1e Chi-Square Test
result_5 <- chisq.test(df$account_mob == 0, df$fin13_1e == 1, correct=FALSE)
result_5$residuals
result_5 <- result_5$p.value

#fin13_1f Chi-Square Test
result_6 <- chisq.test(df$account_mob == 0, df$fin13_1f == 1, correct=FALSE)
result_6$residuals
result_6 <- result_6$p.value


#Filtered fin13_1f Chi-Square Test
result_6b <- chisq.test(df$account_mob == 0, df$fin13_1f == 1 & df$mobileowner != 1, correct=FALSE)
result_6b$residuals
result_6b <- result_6b$p.value






