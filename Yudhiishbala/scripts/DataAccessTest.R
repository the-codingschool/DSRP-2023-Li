library(dplyr)
dataframe <- read.csv("data/DatabankWide Dataset - Data.csv")
#View(dataframe)

micro139 <- readRDS("data/micro_world_139countries.RDS")
#View(micro139)

#df <- filter(micro139, educ != 4, educ != 5)
#View(df)

df <- select(micro139, -economycode)
df <- df %>% select("economy":"account_mob","fin10_1a":"fin10_1e","fin11_1","fin11a":"fin11h","fin13_1a":"fin13_1f")
df$regionwb[df$regionwb == ""] <- "High Income"
df$regionwb[df$regionwb == "East Asia & Pacific (excluding high income)"] <- "East Asia & Pacific"
df$regionwb[df$regionwb == "Europe & Central Asia (excluding high income)"] <- "Europe & Central Asia"
df$regionwb[df$regionwb == "High income"] <- "High Income"
df$regionwb[df$regionwb == "Latin America & Caribbean (excluding high income)"] <- "Latin America & Caribbean"
df$regionwb[df$regionwb == "Middle East & North Africa (excluding high income)"] <- "Middle East & North Africa"
df$regionwb[df$regionwb == "Sub-Saharan Africa (excluding high income)"] <- "Sub-Saharan Africa"
#View(df)

library(ggplot2)
library(stringr)
library(ggthemes)
AccountOwner <- factor(df$account)
ggplot(data = df, aes(x = regionwb,
                      fill = AccountOwner,
                      label = after_stat(count))) +
  scale_fill_manual(name = "Owns Account?",
                    labels = c("No","Yes"),
                    values = c("#ab2b2b","#2da661")) +
  geom_bar(position = "dodge") +
  theme_wsj()+
  theme(plot.title = element_text(size = 26),
        axis.title = element_text(size = 15),
        axis.text = element_text(vjust = 0.9, size = 10, color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)) +
  labs(x = "Region",
       y = "Number of People",
       title = "Number of People per Region with Money Accounts") +
  scale_x_discrete(labels = function(x)stringr::str_wrap(x, width = 15)) +
  geom_text(position = position_dodge2(width = 1, preserve = "single"),
            stat = "count",
            size = 4,
            vjust = -0.5,
            hjust = 0.5)

dataf <- filter(df, account == 0)
dataf <- filter(dataf, !is.na(urbanicity_f2f))
ruralOrNot <- factor(dataf$urbanicity_f2f)
ggplot(data = dataf, aes(x = regionwb,
                         fill = ruralOrNot,
                         label = after_stat(count))) +
  scale_fill_manual(name = "Location",
                    labels = c("Rural","Urban"),
                    values = c("#118f37","#3e4a82")) +
  geom_bar(position = "dodge") +
  theme_wsj()+
  theme(plot.title = element_text(size = 26),
        axis.title = element_text(size = 15),
        axis.text = element_text(vjust = 0.9, size = 10, color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        plot.subtitle = element_text(size = 14)) +
  labs(x = "Region",
       y = "Number of People",
       title = "Rural vs Urban Non-Account Holders",
       subtitle = "Excludes 6905 NA Responses") +
  scale_x_discrete(labels = function(x)stringr::str_wrap(x, width = 15)) +
  geom_text(position = position_dodge2(width = 1, preserve = "single"),
            stat = "count",
            size = 4,
            vjust = -0.5,
            hjust = 0.5)
