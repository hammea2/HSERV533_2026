#If you have not already loaded in your data set (Use your file pathway!)
birthdata_2021 <- read.csv("C:/Users/Owner/OneDrive - UW/HSERV 533/Win 2023/birthdata_2021.csv")

install.packages("table1")
#Note: You only need to install the package once, but you WILL need to run the library comment each time you OPEN R
library(table1)

#Example table 1
##Factor variables are CATEGORICAL variables-> Tells R that categories 
##If there is no "factor()", then the variable is treated as continuous 
## Variable to the right of the "|" is your grouping variable 
##You can add or change these variables below
table1(~ factor(sex) + age_mom + factor(educ_mom) + priorprg | SRpartic, data=birthdata_2021)

