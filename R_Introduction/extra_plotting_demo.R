# Load ggplot2 R package
library(ggplot2)

# Load Case 3 dataset
birthdata_cophp <- read.csv("/home/jovyan/HSERV_533/Case_3/birthdata_cophp.csv")

############################## Basic plots #####################################

# Histograms
## Base R function [hist()]
hist(birthdata_cophp$wt_grams,
     xlab = "Birth weight (g)",
     main = NULL)

hist(birthdata_cophp$wt_grams,
     xlab = "Birth weight (g)",
     main = NULL, breaks = 25)

## ggplot2 [geom_histogram()]
ggplot(data = birthdata_cophp, aes(x = wt_grams))+
  geom_histogram()+
  xlab("Birth weight (g)") +
  theme_classic()

ggplot(data = birthdata_cophp, aes(x = wt_grams))+
  geom_histogram(bins = 25)+
  xlab("Birth weight (g)") +
  theme_classic()

# Boxplots
## Base R function [boxplot()]
boxplot(birthdata_cophp$wt_grams,
     ylab = "Birth weight (g)",
     main = NULL)

## ggplot2 [geom_boxplot())]
ggplot(data = birthdata_cophp, aes(y = wt_grams))+
  geom_boxplot()+
  ylab("Birth weight (g)") +
  theme_classic()

# Scatter plots
## Base R function [plot()]
plot(birthdata_cophp$wt_grams ~ birthdata_cophp$age_mom,
     ylab = "Birth weight (g)",
     xlab = "Mother's age (years)")

## ggplot2 [geom_point()]
ggplot(data = birthdata_cophp, aes(y=wt_grams, x = age_mom))+
  geom_point()+
  ylab("Birth weight (g)")+
  xlab("Mother's age (years)") +
  theme_classic()


############################## Advanced plots ##################################

## Histograms for multiple groups
ggplot(data = birthdata_cophp, aes(x = wt_grams, fill = as.factor(group)))+
  geom_histogram(alpha = 0.6)+
  labs(x = "Birth weight (g)", fill = "SR county") +
  theme_classic()

## Boxplots for multiple groups
ggplot(data = birthdata_cophp, aes(y = wt_grams, fill =  as.factor(group)))+
  geom_boxplot()+
  labs(y = "Birth weight (g)", fill = "SR county") +
  theme_classic()


## Violin plot (fancy type of hybrid histogram/boxplot) for multiple groups
ggplot(data = birthdata_cophp, aes(y = wt_grams, x = as.factor(group), fill = as.factor(group)))+
  geom_violin(draw_quantiles = c(0.25,0.5,0.75))+
  labs(y = "Birth weight (g)", x = "SR county", fill = "SR county") +
  theme_classic()



