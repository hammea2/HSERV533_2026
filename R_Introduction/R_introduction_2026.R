#---------------------------------------------------------------------------------------#
### HSERV 533 ###
### Introduction to R ###
#---------------------------------------------------------------------------------------#

## Data types ##

#---------------------------------------------------------------------------------------#
## Data structures ##
## 1. Vector 
vector() # an empty  vector. The default is 'logical'

#You can also create vectors by directly specifying their content using the c() aka "combine" function.
 
#R will then guess the appropriate data type for the vector based on the data type of the input elements. 
#For instance:
x <- c(1, 2, 3) # Numeric data
y <- c(TRUE, TRUE, FALSE, FALSE) # Logical data
z <- c("Sarah", "Tracy", "Jon") # Character data

## 2. Matrices
# Let's create an empty matrix of 2 columns and 2 rows
m <- matrix(nrow = 2, ncol = 2)
m

# Now let's create a matrix of 3 columns and 2 rows with numeric elements 
mdat <- matrix(c(1, 2, 3, 11, 12, 13),
               nrow = 2,
               ncol = 3,
               byrow = TRUE) #You can use the byrow argument to specify how the matrix is filled
mdat

## 3. Lists
xl <- list(1, "a", TRUE, 3.14159)
xl

## 4. Data frames
dat <- data.frame(id = letters[1:10], x = 1:10, y = 11:20) 
dat
# id, x, y = are the names of the variables in the data frame
# letters[1:10], 1:10, 11:20 = are the type of values within each variable 
#---------------------------------------------------------------------------------------#

## Missing values
xm <- c("a", NA, "c", "d", NA)
ym <- c("a", "b", "c", "d", "e")
is.na(xm)
is.na(ym)
#---------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------#

## Clearing the workspace 
rm(list=ls()) 

## Setting the working directory (The place where you have the datasets stored on the computer you are working on)
setwd("/home/jovyan/HSERV_533/R_Introduction/") # This is the folder (file directory) where course data sets are stored in the JupyterHub environment

## Reading in your data set

# Excel dataset
library(readxl) # This is a R package for loading excel files into R (if you are using your own computer you will first need to install the package the first time you use with "install.packages(readxl)")
pd <- read_excel("WorldPop.xlsx", col_names = TRUE) # This is a function in the readxl package that loads excel files into R

# Reading in the same dataset using using its full path (location on the computer) instead of just the file name
pd_path <- read_excel("/home/jovyan/HSERV_533/R_Introduction/WorldPop.xlsx", col_names = TRUE)

# Reading a the same dataset but stored as a comma-separated (.csv) file
pd_csv <- read.csv("/home/jovyan/HSERV_533/R_Introduction/WorldPop.csv", stringsAsFactors = FALSE)

#---------------------------------------------------------------------------------------#
##Exploring the dataset
# view the dataset 
View(pd)

#view dataset information 
names(pd) #list the variables names 
ncol(pd)  #gives the number of the columns 
nrow(pd)  #gives the number of the rows
dim(pd)   #gives the numbers of the columns and rows
str(pd)   #summarizes the structure of the object, giving the class, dimensions, variable names, data types and a data preview 

#---------------------------------------------------------------------------------------#
## Summary statistics and frequency tables 

# Continuous variable
mean(pd$imr)
sd(pd$imr) 

# Summary statistics and missing values
pd_missing <- read_excel("WorldPop_missing.xlsx")
mean(pd_missing$le) # R is returning NA for the mean because the variable has missing values
sd(pd_missing$le)   # R is returning NA for the standard deviation because the variable has missing values

# In order to overcome this issue, we need to use the argument ns.rm=TRUE 
mean(pd_missing$le, na.rm = TRUE) 
sd(pd_missing$le, na.rm = TRUE)

# The summary function 
summary(pd)
summary(pd_missing)
summary(pd$imr)

# Categorical variable 
table(pd$area)
# To construct and a two-way frequency table with two variables 
table(pd$area, pd$closest_ocean, deparse.level = 2 )

# Frequency table with missing values 
table(pd_missing$area, pd_missing$closest_ocean, deparse.level = 2)
table(pd_missing$area, pd_missing$closest_ocean, deparse.level = 2, useNA = "ifany" )

# Proportion tables 
tableA <- table(pd_missing$area, pd_missing$closest_ocean)
tableA
prop.table(tableA)
prop.table(tableA)*100 # multiply by 100 to get percentages 
round(prop.table(tableA),4)*100 # to round all percentages in the table to 2 decimal places 
 
# To get column percentages, uses margin=2
prop.table(tableA, margin = 2)*100 
round(prop.table(tableA, margin = 2),4)*100 

# To get row percentages, uses margin=1
prop.table(tableA, margin = 1)*100 
round(prop.table(tableA, margin = 1),4)*100 

# Indexing function
# Selecting rows. for example: what are the first 10 observations in the variable pop2012? 
pd$pop2012[1:10]

# Selecting rows by specific columns. For example: what are the values of the first 4 variables in the dataset pd?
pd[,1:4]

# Conditional subsetting
pd.Europe <- pd[pd$area == "Europe",]
pd.Europe

# Conditional re-coding of variables
# Example: re-code the variable imr so that all values of 24 are coded as NA
pd$imr[pd$imr == 24] <- 25

#---------------------------------------------------------------------------------------#
##Summary statistics and frequency tables using packages 

#Install the and psych package 
install.packages("psych") # This package is not already in the JupyterHub environment R package library, so you will need to install it each time you start a new R session

# Load the psych packages into R
library(dplyr)
library(psych)

# dplyr piping (%>%) syntax
# Create a subset of the pd dataset that includes information only on those residing in Africa 
pd %>% filter(area == "Africa")

# Subsetting (filtering) observation from a larger dataset 
Pd.Africa <-pd %>% filter(area == "Africa")
Pd.Africa 

# Subsetting the same observation using Base R syntax
Pd.Africa <- pd[pd$area == "Africa",]

# Selecting variables 
pd.var <- pd %>% select(pop2012, imr)
pd.var

# Selecting same varibale using Base R syntax
pd.base <- pd[,c("pop2012", "imr")]
pd.base

# Keep only observations from Africa and only the variables population and infant mortality rate
# Using dplyr syntax
pd.africa.subs <- pd %>% filter(area == "Africa") %>% select(pop2012, imr)
pd.africa.subs

# Using Base R syntax
pd.africa.base <- pd[pd$area == "Africa",c("pop2012", "imr")]
pd.africa.base

# Create a new variable for whether countries have high vs low infant mortality
pd.mort <- pd %>% mutate(highimr = ifelse(imr > median(imr), 1, 0))
pd.mort

# The mean and median of the population (pop2012), and life expectancy (le) for each region (region). 
pd %>% group_by(region) %>% summarize_each(funs (mean, median), pop2012, imr, le)

# The number of countries, total population, and average population in each region 
# Arranged from largest to smallest (by pop):
pd.reg.pop <- pd %>%
  group_by(region) %>%
  summarize(NumCountries = n(),
            TotalRegPop = sum(pop2012),
            AvgCountPop = mean(pop2012)) %>%
  arrange(desc(TotalRegPop))




























