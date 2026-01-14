# Load R libraries

library(tidyverse)

# We recommend using 'Case5_Short' data set for coding,
# Please look at the "Case 5 Codebook (BRFSS)" for value labels

# If you are not using JupyterHub RStudio, you need to change the path to read the data
case5 <- read.csv("/home/jovyan/HSERV_533/data/Case5_Short.csv") 

### You can do data cleaning and transformations at this stage before running the models

# As an example, below we have converted "Don't know" and "Refused" into NAs for the variable
# AvgDailyDrinks after studying the codebook

case5$AvgDailyDrinks[which(case5$AvgDailyDrinks==77)] <- NA  # converting "Don't know" into NAs
case5$AvgDailyDrinks[which(case5$AvgDailyDrinks==99)] <- NA ## converting "Refused" into NAs


# Another example could be creating any new variables from existing variables as needed
# Below, we created a new categorical variable 'ACE_Cat' from the numeric 'ACE_Count' variable

case5$ACE_Cat <- NA
case5$ACE_Cat[which(case5$ACE_Count<2)] <- 1
case5$ACE_Cat[which(case5$ACE_Count>=2)] <- 2
case5$ACE_Cat[which(case5$ACE_Count>=4)] <- 3

# -----------------------------------------------------------------------------------------

# Day 2 

## Linear Regression (continuous response)----------------------------------
# Fit a linear regression model with PoorMHDays (continuous) as the outcome and ACE_Count (continuous) as the predictor
mod1 <- lm(PoorMHDays ~ ACE_Count, data = case5)
summary(mod1) # Run the summary() function to get the full output 
# Estimates and 95% confidence interval
cbind(coef(mod1), confint(mod1)) %>% round(2) 

# mMke a scatterplot with the fitted linear regression line
ggplot(data = case5,  aes(x = ACE_Count, y = PoorMHDays)) +
  geom_point(alpha = 0.3)+
  geom_smooth(method='lm', formula= y~x, se = FALSE, color = "red") +  # the fitted linear regression line, without confidence interval (se = FALSE)
  xlab("Number of unique types of ACEs") +
  ylab("Days with poor mental health") 

# Find the levels in ACE_Cat
table(case5$ACE_Cat, useNA = "ifany")
# By looking at the Case 5 Codebook (BRFSS), we can figure out what each level represents

# Fit a linear regression with PoorMHDays (continuous) as the response, ACE_Cat (categorical) as the predictor
# Because the values of ACE_Cat are integer, we need to use as.factor(ACE_Cat)
# That way that R will recognize ACE_Cat as a categorical variable.
mod2 <- lm(PoorMHDays ~ as.factor(ACE_Cat), data = case5)
summary(mod2) # Level 0 of ACE_Cat is the reference category
cbind(coef(mod2), confint(mod2)) %>% round(2) 

## Logistic Regression (binary response or a categorical response with two levels) -------------------
# Fit a logistic regression with FMD (binary, 0 and 1) as the response,
# ACE_Count (continuous) as the predictor
mod3 <- glm(FMD ~ ACE_Count, family = binomial(), data = case5)
summary(mod3)
# estimated ORs and 95% CI
cbind(exp(coef(mod3)), exp(confint(mod3))) %>% round(2) ## 

# Fit a logistic regression with FMD (binary) as the response,
# AnyACE (binary/categorical variable) as the predictor
mod4 <- glm(FMD ~ as.factor(AnyACE), family = binomial(), data = case5)
summary(mod4) # Here, level 0 of AnyACE is the reference level
cbind(exp(coef(mod4)), exp(confint(mod4))) %>% round(2)

# Fit a logistic regression with FMD (binary) as the response,
# ACE_Cat (categorical variable) as the predictor
mod5 <- glm(FMD ~ as.factor(ACE_Cat), family = binomial(), data = case5)
summary(mod5) # Here, level 0 of ACE_Cat is the reference level
cbind(exp(coef(mod5)), exp(confint(mod5))) %>% round(2)


# Linear regression adjusted for the respondent sex
mod6 <- lm(PoorMHDays~ACE_Count+as.factor(SEXVAR), data = case5)
summary(mod6) # Run the summary() function to get the full output
# Estimate and 95% Confidence Intervals
cbind(coef(mod6), confint(mod6)) %>% round(2) # the estimates and 95% their confidence interval

# Adjusted logistic regression
mod7 <- glm(FMD~ACE_Count+as.factor(SEXVAR), data = case5, family = binomial())
summary(mod7) #  the logistic model output in natural logarithm 
cbind(exp(coef(mod7)), exp(confint(mod7))) %>% round(2)  # exp(), exponent - the output to get odds ratio

# Subset to Male Respondents
mod8 <- glm(FMD~as.factor(AnyACE), 
            family = binomial(),
            data = subset(case5, SEXVAR == 1))
summary(mod8)
cbind(exp(coef(mod8)), exp(confint(mod8)))%>% round(2)

# Subset to Female Respondents
mod9 <- glm(FMD~as.factor(AnyACE), 
            family = binomial(),
            data = subset(case5, SEXVAR == 2))
summary(mod9)
cbind(exp(coef(mod9)), exp(confint(mod9)))%>% round(2)



# -----------------------------------------------------------------------------------------


# Day 3 
# Make a scatterplot with a fitted linear regression

ggplot(data = case5,  aes(x = AvgDailyDrinks, y = ACE_Count)) +
  geom_point(alpha = 0.3)+
  geom_smooth(method='lm', formula= y~x, se = FALSE, color = "red") +  # the fitted linear regression line, without confidence interval (se = FALSE)
  xlab("Number of daily drinks") +
  ylab("Number of unique types of ACEs") 

# Make a scatterplot with a fitted linear regression
ggplot(data = case5,  aes(x = AvgDailyDrinks, y = PoorMHDays)) +
  geom_point(alpha = 0.3)+
  geom_smooth(method='lm', formula= y~x, se = FALSE, color = "red") +  # the fitted linear regression line, without confidence interval (se = FALSE)
  xlab("Number of daily drinks") +
  ylab("Days with poor mental health") 



## Fit an unadjusted linear regression with PoorMHDays (continuous) as the response, AnyACE (categorical, 0 and 1) as the predictor
mod10 <- lm(PoorMHDays ~ as.factor(AnyACE), data = case5)
summary(mod10) # Level 0 of AnyACE is the reference category
cbind(coef(mod10), confint(mod10))

## A linear regression with PoorMHDays (continuous) as the response, AnyACE (categorical, 0 and 1) as the predictor, 
## adjusting for AvgDailyDrinks (continuous)
mod11 <- lm(PoorMHDays ~ as.factor(AnyACE) + AvgDailyDrinks, data = case5)
summary(mod11) # Level 0 of AnyACE is the reference category
cbind(coef(mod11), confint(mod11))

# R Tips for Day 3 ---------------------------
## Linear regression ---------------
## Unadjusted ------
mod1 <- lm(PoorMHDays ~ ACE_Count, data = case5)
summary(mod1) # Run the summary() function to get the full output 
# Estimates and 95% confidence interval
cbind(coef(mod1), confint(mod1)) %>% round(2) 

## Adjust for AvgDailyDrinks -------
mod11 <- lm(PoorMHDays ~ as.factor(AnyACE) + AvgDailyDrinks, data = case5)
summary(mod11) # Level 0 of AnyACE is the reference category
cbind(coef(mod11), confint(mod11))

## Logistic regression -------------------------
## Unadjusted -------
mod3 <- glm(FMD ~ ACE_Count, family = binomial(), data = case5)
summary(mod3)
cbind(exp(coef(mod3)), exp(confint(mod3)))

## Adjusted for AvgDailyDrinks------
mod12 <- glm(FMD ~ ACE_Count + AvgDailyDrinks, family = binomial(), data = case5)
summary(mod12)
cbind(exp(coef(mod12)), exp(confint(mod12)))


# -----------------------------------------------------------------------------------------


# Day 4 -----------------------------------------------------------------------------------
## Fit the logistic regression on the SEXVAR == 1, i.e. male subset.
mod8 <- glm(FMD~ as.factor(AnyACE), 
            family = binomial(),
            data = subset(case5, SEXVAR == 1))
summary(mod8)
cbind(exp(coef(mod8)), exp(confint(mod8)))

## Fit the logistic regression on the SEXVAR == 2, i.e. female subset.
mod9 <- glm(FMD~as.factor(AnyACE), 
            family = binomial(),
            data = subset(case5, SEXVAR == 2))
summary(mod9)
cbind(exp(coef(mod9)), exp(confint(mod9)))

# -----------------------------------------------------------------------------------------

# Specify a reference level
## By default, R uses the alpha-numerically first category as the reference category (e.g. "a" and "0")
## To specify a reference level, we can use the relevel() function
## (Linear regression)
## Previously, in mod2, we use the default, level 0 as the reference category.
mod2 <- lm(PoorMHDays ~ as.factor(ACE_Cat), data = case5)
summary(mod2_1) # Level 0 of ACE_Cat is the reference category
cbind(coef(mod2), confint(mod2)) %>% round(2) 

# Now we use level 2 as our reference category
case5$ACE_Cat_2 <- relevel(as.factor(case5$ACE_Cat),ref = "2")
# Fit the model
mod2_1 <- lm(PoorMHDays ~ as.factor(ACE_Cat_2), data = case5)
summary(mod2_1) # Level 2 of ACE_Cat is the reference category
cbind(coef(mod2_1), confint(mod2_1)) %>% round(2) 

## We can use the same relevel method to fit a logistic regression
case5$AnyACE_1 <- relevel(as.factor(case5$AnyACE),ref= "1")

mod4_1 <- glm(FMD ~ as.factor(AnyACE_1), family = binomial(), data = case5)
summary(mod4_1) # Here, level 1 of AnyACE is the reference level
cbind(exp(coef(mod4_1)), exp(confint(mod4_1))) %>% round(2)


####### Advanced packages to display regression results (optional) ############
## The gtsummary and broom R packages can simplify displaying the results of regression models
#install.packages(c("gtsummary","broom.helpers")) # you would need to install and load these packages for the advanced examples to work
#library(gtsummary)
#library(broom)

## Example linear regression model: mod2_1

case5$ACE_Cat_2 <- relevel(as.factor(case5$ACE_Cat),ref = "2")
mod2_1 <- lm(PoorMHDays ~ as.factor(ACE_Cat_2), data = case5)

### Standard R output 
summary(mod2_1)
cbind(coef(mod2_1), confint(mod2_1)) %>% round(2) 

### Using the tbl_regression() function from the gt_summary package
tbl_regression(mod2_1)


## Example logistic regression model: mod12
mod12 <- glm(FMD ~ ACE_Count + AvgDailyDrinks, family = binomial(), data = case5)

### Standard R output 
summary(mod12)
cbind(exp(coef(mod12)), exp(confint(mod12)))


### Using the tbl_regression() function from the gt_summary package

# Output on the log(OR) scale
tbl_regression(mod12)

# Output on the OR scale (tbl_regression can automatically exponentiate the coefficients)
tbl_regression(mod12, exponentiate = TRUE)

























































