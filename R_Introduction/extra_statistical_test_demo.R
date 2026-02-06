library(tidyverse)

data <- read.csv("/home/jovyan/HSERV_533/Case_3/birthdata_cophp.csv")

## Two sample z-test ##

## Two-sided test (default settings)
prop.test(table(data$group,data$lbw),
          alternative = "two.sided",
          correct = TRUE,
          conf.level = 0.95) 

## One-sided (greater) test without continuity correction
prop.test(table(data$group,data$lbw),
          alternative = "greater",
          correct = FALSE,
          conf.level = 0.95)

## Two sample t-test ##

## Two-sided test (default settings)
t.test(wt_grams ~ group,
       data = data,
       alternative = "two.sided",
       conf.level = 0.95)

## One-sided (greater) test 
t.test(wt_grams ~ group,
       data = data,
       alternative = "greater",
       conf.level = 0.95,
       var.equal = TRUE)

## Correlation test ##
## Two sided (default settings) ##

cor.test(data$pnatalvs, data$wt_grams,
         alternative = "two.sided",
         method = "pearson",
         conf.level = 0.95)

## Scatter plot of the relationship ##

ggplot(data = data, aes(x = pnatalvs, y = wt_grams, color = lbw))+
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 2500) +
  theme_classic() 


## Chi-squared test ##
table(data$group, data$payer, deparse.level = 2)
chisq.test(table(data$group, data$payer))

table(data$moracsum, data$payer, deparse.level = 2)
chisq.test(table(data$moracsum, data$payer))

## ANOVA ##
aov(data$pnatalvs ~ as.factor(data$payer)) %>%
  summary()

## Boxplots as a visual representation of what the ANOVA is testing

ggplot(data = data, aes(x = as.factor(payer), y = pnatalvs))+
  geom_boxplot() +
  theme_classic()


## OR/RR calculation from contingency table in R (more advanced) ##

# Epi 2x2
install.packages("epiR")
library(epiR)

table(data$group, data$lbw, deparse.level = 2) 

# Relevel so that 2x2 table is formatted correctly
data$group_fct = relevel(as.factor(data$group), ref = 2)
data$lbw_fct= relevel(as.factor(data$lbw), ref = 1)

table(data$group_fct, data$lbw_fct, deparse.level = 2)

epi.2by2(table(data$group_fct, data$lbw_fct, deparse.level = 2),
         method = "cohort.count")

