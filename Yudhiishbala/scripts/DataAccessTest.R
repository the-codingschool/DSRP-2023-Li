library(dplyr)
micro139 <- readRDS("data/micro_world_139countries.RDS")

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
       title = "Rural vs Urban Non-Account Holders") +
  scale_x_discrete(labels = function(x)stringr::str_wrap(x, width = 15)) +
  geom_text(position = position_dodge2(width = 1, preserve = "single"),
            stat = "count",
            size = 4,
            vjust = -0.5,
            hjust = 0.5)

