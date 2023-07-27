microworld <- readRDS("~/Documents/ResearchProject-Li/data/micro_world_139countries.RDS")
View(microworld)
library(ggplot2)
library(corrplot)

###debit
## Null - There is no association between different regions and owning a debit card
## Alternate - There is a relationship between different regions and owning a debit card

microworld_clean <- microworld |>
  filter(! is.na(regionwb), 
         ! is.na(microworld$fin2))


t <- table(microworld$regionwb, microworld$fin2)
chisq.test(t) 


chisq_result <- chisq.test(t)
chisq_result
## X-squared = 42692, df = 21, p-value < 2.2e-16
chisq_result$p.value
chisq_result$residuals

corrplot(chisq_result$residuals, is.cor = F)

### credit
## Null - There is no association between different regions and owning a credit card
## Alternate - There is a relationahip between different regions and owning a credit card

microworld_clean2 <- microworld |>
  filter(! is.na(regionwb), 
         ! is.na(microworld$fin7))


tt <- table(microworld$regionwb, microworld$fin7)
chisq.test(tt) 


chisq_result2 <- chisq.test(tt)
chisq_result2
## X-squared = 12191, df = 21, p-value < 2.2e-16
chisq_result2$p.value
chisq_result2$residuals

corrplot(chisq_result2$residuals, is.cor = F)

