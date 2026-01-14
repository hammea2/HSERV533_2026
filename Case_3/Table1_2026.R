# Install the table1 R package
install.packages("table1")

# Load the table1 R package
library(table1)

# Load data
birthdata_cophp <- read.csv("/home/jovyan/HSERV_533/Case_3/birthdata_cophp.csv") # Change the file path if you are not using JupyterHub RStudio

# Example Table 1
## Factor variables are categorical variables -> Lets R know that we have categories even if the values are numerical
## If a variable has multiple numeric values then table1 treats the variable as continuous by default
## Variable to the right of the "|" is your grouping variable 
## You can add or change to the variables in the example table  below

table1(~ sex + age_mom + factor(educ_mom) + priorprg | SRpartic, data=birthdata_cophp)

# You can try constructing your own Table 1 using different variables

############## Advanced table1 formatting (optional) #########################################
## You can make your Table 1 look nicer by giving R descriptive names for the variables using the data dictionary
## Your can set these variable descriptive lables using the label() function
label(birthdata_cophp$age_mom) <- "Mother's Age"
label(birthdata_cophp$priorprg) <- "Total Prior Pregnancies"

## For binary and categorical variables you might also consider giving the levels of the variables descriptive labels

### This code creates new variables with more descriptive values for use in the table 
birthdata_cophp$descriptive_sex <- ifelse(birthdata_cophp$sex == "F", "Female","Male")
birthdata_cophp$descriptive_SRpartic <- ifelse(birthdata_cophp$SRpartic == 1, "Starting Right program participant", "Non-participants")

label(birthdata_cophp$descriptive_sex) <- "Sex of Child"
label(birthdata_cophp$descriptive_SRpartic) <- "Whether Starting Right program participant or not"

birthdata_cophp$educ_mom_descriptive_factor = factor(birthdata_cophp$educ_mom, levels = c(1:8),
                                         labels = c("8th grade or less",
                                                  "9th-12th grade, no diploma",
                                                  "High school graduate or GED",
                                                  "Some college, no degree",
                                                  "Associate degree",
                                                  "Bachelor’s degree",
                                                  "Master’s degree",
                                                  "Doctorate or Professional degree"))

label(birthdata_cophp$educ_mom_descriptive_factor) <- "Mother's Education"

# Example Table1 with descriptive variable and value labels
table1(~ descriptive_sex + age_mom + educ_mom_descriptive_factor + priorprg | descriptive_SRpartic, data=birthdata_cophp)



