# Case 3 R script ---------------------------------------------------------------------------------

## Install the psych R library (if needed)
install.packages("psych")

## Load R libraries
library(tidyverse)
library(psych)

# Load dataset
birthdata_cophp <- read.csv("/home/jovyan/HSERV_533/Case_3/birthdata_cophp.csv")
## Note that if you are not using this script on JupyterHub RStudio you will need to change the file path.

# Look at the first 6 observations of the data
head(birthdata_cophp)
glimpse(birthdata_cophp) # dplyr alternative to head

# Check the summary statistics for all variables in the data
summary(birthdata_cophp) # Using the base R summary() function
describe(birthdata_cophp) # Using the psych R package describe() function
  
# Look at the statistics of birthweight variable
summary(birthdata_cophp$wt_grams) # Using the base R summary() function
describe(birthdata_cophp$wt_grams) # Using the psych R package describe() function

# For binary variables it usually makes more sense to construct a contingency table or proportions table
table(birthdata_cophp$sex) # Using the base R table() function
table(birthdata_cophp$sex) %>% prop.table() %>% round(3) # Piping the output contigency table to the prop.table() function to construct a proportion table and then rounding to 3 decimal places using the round() function

# Try taking a look at the distributions of some other relevant variables

# Day  2 ------------------------------------------------------------------------------------------------------

# Provide the summary statistics of birthweight (a continuous variable) for people living in a Starting Right county. 
## Birth weight variable contains missing observations (NA). We need to remove missingness in order to calculate the summary statistics. 
## At the same time, we should also provide the number of missingness.
birthdata_cophp %>% filter(group == 1) %>% 
  summarise(Mean = mean(wt_grams, na.rm = TRUE), 
            SD = sd(wt_grams, na.rm = TRUE),
            Min = min(wt_grams, na.rm = TRUE),
            Max = max(wt_grams, na.rm = TRUE),
            Missing = sum(is.na(wt_grams))) 

# Provide the summary statistics of birthweight (a  continuous variable) based on whether people living in a Starting Right county or not.
birthdata_cophp %>% group_by(group) %>% 
  summarise(Mean = mean(wt_grams, na.rm = TRUE), 
            SD = sd(wt_grams, na.rm = TRUE),
            Min = min(wt_grams, na.rm = TRUE),
            Max = max(wt_grams, na.rm = TRUE),
            Missing = sum(is.na(wt_grams)))

# Provide the summary statistics of sex by the status of living in a Starting Right county
## Sex is a nominal variable with two levels, F and M
birthdata_cophp %>% group_by(group) %>% 
  summarise(n.F = sum(sex == "F"), # F is a character, we need to quote F. 
            prop.F = round(mean(sex == "F"),2), ## round up to 2 decimal points
            n.M = sum(sex == "M"),
            prop.M = round(mean(sex == "M"),2))

# Create a binary variable, lbw, with two levels, < 2500 and >= 2500 birthweight
birthdata_cophp$lbw <- NA # This line creates a new variable named "lbw" that contains all NA values
birthdata_cophp$lbw[birthdata_cophp$wt_grams < 2500] <- 0 # We can then replace the NA values with 0 if the infant is < 2,500g at birth
birthdata_cophp$lbw[birthdata_cophp$wt_grams >= 2500] <- 1 # Or with 1 if the infant is >= 2,500g at birth

# Draw a histogram
hist(birthdata_cophp$lbw)

# Display multiple histograms in one page
par(mfrow = c(2, 3))  # Set the display format for the plot panel: 2 rows (first number) and 3 columns (second number)
hist(birthdata_cophp$wt_grams, xlab = "Birth weight in gram", main = "Birth weight in gram")
hist (birthdata_cophp$birth_hr, xlab = "Time of birth in 24-hour", main = "Time of birth in 24-hour")
hist(birthdata_cophp$gestcalc, xlab = "Gestational age at birth", main = "Gestational age at birth" )
hist(birthdata_cophp$age_mom, xlab = "Age of mothers in years", main = "Age of mothers in years")
hist(birthdata_cophp$priorprg, xlab = "Total number of prior pregnancies", main ="Total number of prior pregnancy")
hist(birthdata_cophp$mat_height, xlab = "Maternal height", main = "Maternal height")

# Day 3 ------------------------------------------------------------------------------------------------------------------------

# Construct a 2 by 2 table (two category variables)
table(birthdata_cophp$lbw,birthdata_cophp$sex)

# t-test ------------------------------------------
# A two-sample t-test: a continuous dependent variable (y) and a binary independent variable (x), y ~ x
# Comparing the mean difference on birthweight between females (1) and males (0)
t.test(birthdata_cophp$wt_grams ~ birthdata_cophp$female, var.equal = TRUE)

# A one-sample t-test: a continuous variable and a known constant
# Comparing the mean birth weight with 3389 
t.test(birthdata_cophp$wt_grams, mu=3389, na.rm=TRUE)

# z-test ------------------------------------------
# A two-sample z-test: two binary variables
# Comparing the proportion of low birthweight infant among females and the proportion of low birthweight infant among males
lbw_female <- table(birthdata_cophp$female, birthdata_cophp$lbw) # Here we just construct the 2 by 2 table ahead of time
prop.test(lbw_female) # Then we use it as input for the prop.test(function)


# Another way to perform the same test 
prop.test(table(birthdata_cophp$female, birthdata_cophp$lbw)) # We now construct the 2 by 2 table inside of the prop.test() function

# A one sample z-test: comparing a proportion with a known percent
# Comparing the proportion of low birthweight infants with a null proportion of 10%
table(birthdata_cophp$lbw)
prop.test(36, n = 990, p = 0.1, correct = FALSE)

# Day 4 ------------------------------------------------------------------------------------------------------------------------

# A chi-square test tests the association between two categorical variables (pvisit_cat and payer)
tab_chi <- table(birthdata_cophp$pvisit_cat, birthdata_cophp$payer)
chisq.test(tab_chi)

# An ANOVA test: a continuous dependent variable (y, pnatalvs) and a nominal independent variable (x, payer), y ~ x
one.way <- aov(birthdata_cophp$pnatalvs ~ as.factor(birthdata_cophp$payer))
summary(one.way)

# A correlation test: measures the correlation between two continuous variables
cor.test(birthdata_cophp$wt_grams, birthdata_cophp$gestcalc)



























