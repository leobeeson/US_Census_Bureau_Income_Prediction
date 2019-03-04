library(dplyr)
library(arsenal)

##### READ IN TRAIN AND TEST SET #####
train_data <- readRDS(file = "data/train_data.RDS")
test_data <- readRDS(file = "data/test_data.RDS")

##### CREATE COPY OF TRAIN AND TEST SET FOR POSTERIOR ANALYSIS #####
copy_train_data <- train_data
copy_test_data <- test_data

# Identify categorical values:
cat_vars_train <- unname(sapply(train_data, class)) == "factor"
cat_vars_test <- unname(sapply(test_data, class)) == "factor"
all(cat_vars_train == cat_vars_test) # [1] TRUE

# Convert categorical variables to character:
train_data[,cat_vars_train] <- sapply(train_data[,cat_vars_train], as.character)
test_data[,cat_vars_test] <- sapply(test_data[,cat_vars_test], as.character)


#################################
##### RECODING: Trainin Set #####
#################################


#####################################
##### RECODING: class_of_worker #####
#####################################

# Initial NIU in variable: 
length(copy_train_data$class_of_worker[copy_train_data$class_of_worker == "NIU"]) # [1] 100245  
length(copy_train_data$class_of_worker[copy_train_data$class_of_worker == "NIU"]) / nrow(train_data) # Prop: 0.5024233

# Assign NIU of people below legal working age (Unrestricted by federal law) to factor level "Never worked": https://en.wikipedia.org/wiki/Legal_working_age
length(train_data$class_of_worker[train_data$class_of_worker == "NIU" & train_data$age < 16]) # [1] 49916
train_data$class_of_worker[train_data$class_of_worker == "NIU" & train_data$age < 16] <- "Never worked"

# Assign NIU of people above retirement age to NEW factor level "Pass retirement":
length(train_data$class_of_worker[train_data$class_of_worker == "NIU" & train_data$age > 64]) # [1] 21191
train_data$class_of_worker[train_data$class_of_worker == "NIU" & train_data$age > 64] <- "Pass retirement"

# Assign NIU of people of working age which are not in the labour force to NEW factor level "Not in labor force":
length(train_data$class_of_worker[train_data$class_of_worker == "NIU" & train_data$full_or_part_time_employment_stat == "Not in labor force"]) # [1] 14226
train_data$class_of_worker[train_data$class_of_worker == "NIU" & train_data$full_or_part_time_employment_stat == "Not in labor force"] <- "Not in labor force"

# Assign NIU of people of working age which did not work that year to NEW factor level "Not in labor force":
length(train_data$class_of_worker[train_data$class_of_worker == "NIU" & train_data$weeks_worked_in_year == 0]) # [1] 11051
train_data$class_of_worker[train_data$class_of_worker == "NIU" & train_data$weeks_worked_in_year == 0] <- "Not in labor force"

# Remaining NIU in variable: 
length(train_data$class_of_worker[train_data$class_of_worker == "NIU"]) # [1] 3861  
length(train_data$class_of_worker[train_data$class_of_worker == "NIU"]) / nrow(train_data) # Prop: 0.01935115


######################################################################################################################
##### RECODING: detailed_industry_recode, detailed_occupation_recode, major_industry_code, major_occupation_code #####
######################################################################################################################

# Initial NIU in variables:
length(copy_train_data$detailed_industry_recode[copy_train_data$detailed_industry_recode == 0]) # [1] 100684
length(copy_train_data$detailed_occupation_recode[copy_train_data$detailed_occupation_recode == 0]) # [1] 100684
length(copy_train_data$major_industry_code[copy_train_data$major_industry_code == "NIU"]) # [1] 100684
length(copy_train_data$major_occupation_code[copy_train_data$major_occupation_code == "NIU"]) # [1] 100684
length(copy_train_data$major_occupation_code[copy_train_data$major_occupation_code == "NIU"])/nrow(train_data) # Prop: 0.5046235

# Assign NIU of people below legal working age to factor level "Never worked":
length(train_data$detailed_industry_recode[train_data$detailed_industry_recode == 0 & train_data$age < 16]) # [1] 49960
train_data$detailed_industry_recode[train_data$detailed_industry_recode == 0 & train_data$age < 16] <- "Never worked"

length(train_data$detailed_occupation_recode[train_data$detailed_occupation_recode == 0 & train_data$age < 16]) # [1] 49960
train_data$detailed_occupation_recode[train_data$detailed_occupation_recode == 0 & train_data$age < 16] <-  "Never worked"

length(train_data$major_industry_code[train_data$major_industry_code == "NIU" & train_data$age < 16]) # [1] 49960
train_data$major_industry_code[train_data$major_industry_code == "NIU" & train_data$age < 16] <-  "Never worked"

length(train_data$major_occupation_code[train_data$major_occupation_code == "NIU" & train_data$age < 16]) # [1] 49960
train_data$major_occupation_code[train_data$major_occupation_code == "NIU" & train_data$age < 16] <-  "Never worked"

# Assign NIU of people above retirement age to NEW factor level "Pass retirement":
length(train_data$detailed_industry_recode[train_data$detailed_industry_recode == 0 & train_data$age > 64]) # [1] 21191
train_data$detailed_industry_recode[train_data$detailed_industry_recode == 0 & train_data$age > 64] <- "Pass retirement"

length(train_data$detailed_occupation_recode[train_data$detailed_occupation_recode == 0 & train_data$age > 64]) # [1] 21191
train_data$detailed_occupation_recode[train_data$detailed_occupation_recode == 0 & train_data$age > 64] <-  "Pass retirement"

length(train_data$major_industry_code[train_data$major_industry_code == "NIU" & train_data$age > 64]) # [1] 21191
train_data$major_industry_code[train_data$major_industry_code == "NIU" & train_data$age > 64] <-  "Pass retirement"

length(train_data$major_occupation_code[train_data$major_occupation_code == "NIU" & train_data$age > 64]) # [1] 21191
train_data$major_occupation_code[train_data$major_occupation_code == "NIU" & train_data$age > 64] <-  "Pass retirement"

# Assign NIU of people of working age which are not in the labour force to NEW factor level "Not in labor force":
length(train_data$detailed_industry_recode[train_data$detailed_industry_recode == 0 & train_data$full_or_part_time_employment_stat == "Not in labor force"]) # [1] 14226
train_data$detailed_industry_recode[train_data$detailed_industry_recode == 0 & train_data$full_or_part_time_employment_stat == "Not in labor force"] <- "Not in labor force"

length(train_data$detailed_occupation_recode[train_data$detailed_occupation_recode == 0 & train_data$full_or_part_time_employment_stat == "Not in labor force"]) # [1] 14226
train_data$detailed_occupation_recode[train_data$detailed_occupation_recode == 0 & train_data$full_or_part_time_employment_stat == "Not in labor force"] <- "Not in labor force"

length(train_data$major_industry_code[train_data$major_industry_code == "NIU" & train_data$full_or_part_time_employment_stat == "Not in labor force"]) # [1] 14226
train_data$major_industry_code[train_data$major_industry_code == "NIU" & train_data$full_or_part_time_employment_stat == "Not in labor force"] <- "Not in labor force"

length(train_data$major_occupation_code[train_data$major_occupation_code == "NIU" & train_data$full_or_part_time_employment_stat == "Not in labor force"]) # [1] 14226
train_data$major_occupation_code[train_data$major_occupation_code == "NIU" & train_data$full_or_part_time_employment_stat == "Not in labor force"] <- "Not in labor force"

# Assign NIU of people of working age which did not work that year to NEW factor level "Not in labor force":
length(train_data$detailed_industry_recode[train_data$detailed_industry_recode == 0 & train_data$weeks_worked_in_year == 0]) # [1] 11408
train_data$detailed_industry_recode[train_data$detailed_industry_recode == 0 & train_data$weeks_worked_in_year == 0] <- "Not in labor force"

length(train_data$detailed_occupation_recode[train_data$detailed_occupation_recode == 0 & train_data$weeks_worked_in_year == 0]) # [1] 11408
train_data$detailed_occupation_recode[train_data$detailed_occupation_recode == 0 & train_data$weeks_worked_in_year == 0] <- "Not in labor force"

length(train_data$major_industry_code[train_data$major_industry_code == "NIU" & train_data$weeks_worked_in_year == 0]) # [1] 11408
train_data$major_industry_code[train_data$major_industry_code == "NIU" & train_data$weeks_worked_in_year == 0] <- "Not in labor force"

length(train_data$major_occupation_code[train_data$major_occupation_code == "NIU" & train_data$weeks_worked_in_year == 0]) # [1] 11408
train_data$major_occupation_code[train_data$major_occupation_code == "NIU" & train_data$weeks_worked_in_year == 0] <- "Not in labor force"

# Remaining NIU in variables: 
length(train_data$major_occupation_code[train_data$major_occupation_code == "NIU"]) # [1] 3899
length(train_data$major_occupation_code[train_data$major_occupation_code == "NIU"])/nrow(train_data) # Prop: 0.01954161

#############################################################################################
##### RECODING: country_of_birth_self, country_of_birth_father, country_of_birth_mother #####
#############################################################################################

# Assign NIU of father's country of origin to mother's, and viceversa.
# Assumption: 93% of parents are born in the same country:
sum(train_data$country_of_birth_father == train_data$country_of_birth_mother)/nrow(train_data) # [1] 0.9296522
# For country of father:
length(train_data$country_of_birth_father[train_data$country_of_birth_father == "NA" & train_data$country_of_birth_mother != "NA"]) # [1] 1834
train_data$country_of_birth_father[train_data$country_of_birth_father == "NA" & train_data$country_of_birth_mother != "NA"] <- 
  train_data$country_of_birth_mother[train_data$country_of_birth_father == "NA" & train_data$country_of_birth_mother != "NA"]
# For country of mother:
length(train_data$country_of_birth_mother[train_data$country_of_birth_mother == "NA" & train_data$country_of_birth_father != "NA"]) # [1] 1240
train_data$country_of_birth_mother[train_data$country_of_birth_mother == "NA" & train_data$country_of_birth_father != "NA"] <- 
  train_data$country_of_birth_father[train_data$country_of_birth_mother == "NA" & train_data$country_of_birth_father != "NA"]


##############################
##### RECODING: Test Set #####
##############################


#####################################
##### RECODING: class_of_worker #####
#####################################

# Initial NIU in variable: 
length(copy_test_data$class_of_worker[copy_test_data$class_of_worker == "NIU"]) # [1] 50079
length(copy_test_data$class_of_worker[copy_test_data$class_of_worker == "NIU"]) / nrow(test_data) # Prop: 0.5019847

# Assign NIU of people below legal working age (Unrestricted by federal law) to factor level "Never worked": https://en.wikipedia.org/wiki/Legal_working_age
length(test_data$class_of_worker[test_data$class_of_worker == "NIU" & test_data$age < 16]) # [1] 24698
test_data$class_of_worker[test_data$class_of_worker == "NIU" & test_data$age < 16] <- "Never worked"

# Assign NIU of people above retirement age to NEW factor level "Pass retirement":
length(test_data$class_of_worker[test_data$class_of_worker == "NIU" & test_data$age > 64]) # [1] 10829
test_data$class_of_worker[test_data$class_of_worker == "NIU" & test_data$age > 64] <- "Pass retirement"

# Assign NIU of people of working age which are not in the labour force to NEW factor level "Not in labor force":
length(test_data$class_of_worker[test_data$class_of_worker == "NIU" & test_data$full_or_part_time_employment_stat == "Not in labor force"]) # [1] 7046
test_data$class_of_worker[test_data$class_of_worker == "NIU" & test_data$full_or_part_time_employment_stat == "Not in labor force"] <- "Not in labor force"

# Assign NIU of people of working age which did not work that year to NEW factor level "Not in labor force":
length(test_data$class_of_worker[test_data$class_of_worker == "NIU" & test_data$weeks_worked_in_year == 0]) # [1] 5534
test_data$class_of_worker[test_data$class_of_worker == "NIU" & test_data$weeks_worked_in_year == 0] <- "Not in labor force"

# Remaining NIU in variable: 
length(test_data$class_of_worker[test_data$class_of_worker == "NIU"]) # [1] 1972
length(test_data$class_of_worker[test_data$class_of_worker == "NIU"]) / nrow(test_data) # Prop: 0.01976705


######################################################################################################################
##### RECODING: detailed_industry_recode, detailed_occupation_recode, major_industry_code, major_occupation_code #####
######################################################################################################################

# Initial NIU in variables:
length(copy_test_data$detailed_industry_recode[copy_test_data$detailed_industry_recode == 0]) # [1] 50283
length(copy_test_data$detailed_occupation_recode[copy_test_data$detailed_occupation_recode == 0]) # [1] 50283
length(copy_test_data$major_industry_code[copy_test_data$major_industry_code == "NIU"]) # [1] 50283
length(copy_test_data$major_occupation_code[copy_test_data$major_occupation_code == "NIU"]) # [1] 50283
length(copy_test_data$major_occupation_code[copy_test_data$major_occupation_code == "NIU"])/nrow(test_data) # Prop: 0.5040296

# Assign NIU of people below legal working age to factor level "Never worked":
length(test_data$detailed_industry_recode[test_data$detailed_industry_recode == 0 & test_data$age < 16]) # [1] 24725
test_data$detailed_industry_recode[test_data$detailed_industry_recode == 0 & test_data$age < 16] <- "Never worked"

length(test_data$detailed_occupation_recode[test_data$detailed_occupation_recode == 0 & test_data$age < 16]) # [1] 24725
test_data$detailed_occupation_recode[test_data$detailed_occupation_recode == 0 & test_data$age < 16] <-  "Never worked"

length(test_data$major_industry_code[test_data$major_industry_code == "NIU" & test_data$age < 16]) # [1] 24725
test_data$major_industry_code[test_data$major_industry_code == "NIU" & test_data$age < 16] <-  "Never worked"

length(test_data$major_occupation_code[test_data$major_occupation_code == "NIU" & test_data$age < 16]) # [1] 24725
test_data$major_occupation_code[test_data$major_occupation_code == "NIU" & test_data$age < 16] <-  "Never worked"

# Assign NIU of people above retirement age to NEW factor level "Pass retirement":
length(test_data$detailed_industry_recode[test_data$detailed_industry_recode == 0 & test_data$age > 64]) # [1] 10829
test_data$detailed_industry_recode[test_data$detailed_industry_recode == 0 & test_data$age > 64] <- "Pass retirement"

length(test_data$detailed_occupation_recode[test_data$detailed_occupation_recode == 0 & test_data$age > 64]) # [1] 10829
test_data$detailed_occupation_recode[test_data$detailed_occupation_recode == 0 & test_data$age > 64] <-  "Pass retirement"

length(test_data$major_industry_code[test_data$major_industry_code == "NIU" & test_data$age > 64]) # [1] 10829
test_data$major_industry_code[test_data$major_industry_code == "NIU" & test_data$age > 64] <-  "Pass retirement"

length(test_data$major_occupation_code[test_data$major_occupation_code == "NIU" & test_data$age > 64]) # [1] 10829
test_data$major_occupation_code[test_data$major_occupation_code == "NIU" & test_data$age > 64] <-  "Pass retirement"

# Assign NIU of people of working age which are not in the labour force to NEW factor level "Not in labor force":
length(test_data$detailed_industry_recode[test_data$detailed_industry_recode == 0 & test_data$full_or_part_time_employment_stat == "Not in labor force"]) # [1] 7046
test_data$detailed_industry_recode[test_data$detailed_industry_recode == 0 & test_data$full_or_part_time_employment_stat == "Not in labor force"] <- "Not in labor force"

length(test_data$detailed_occupation_recode[test_data$detailed_occupation_recode == 0 & test_data$full_or_part_time_employment_stat == "Not in labor force"]) # [1] 7046
test_data$detailed_occupation_recode[test_data$detailed_occupation_recode == 0 & test_data$full_or_part_time_employment_stat == "Not in labor force"] <- "Not in labor force"

length(test_data$major_industry_code[test_data$major_industry_code == "NIU" & test_data$full_or_part_time_employment_stat == "Not in labor force"]) # [1] 7046
test_data$major_industry_code[test_data$major_industry_code == "NIU" & test_data$full_or_part_time_employment_stat == "Not in labor force"] <- "Not in labor force"

length(test_data$major_occupation_code[test_data$major_occupation_code == "NIU" & test_data$full_or_part_time_employment_stat == "Not in labor force"]) # [1] 7046
test_data$major_occupation_code[test_data$major_occupation_code == "NIU" & test_data$full_or_part_time_employment_stat == "Not in labor force"] <- "Not in labor force"

# Assign NIU of people of working age which did not work that year to NEW factor level "Not in labor force":
length(test_data$detailed_industry_recode[test_data$detailed_industry_recode == 0 & test_data$weeks_worked_in_year == 0]) # [1] 5695
test_data$detailed_industry_recode[test_data$detailed_industry_recode == 0 & test_data$weeks_worked_in_year == 0] <- "Not in labor force"

length(test_data$detailed_occupation_recode[test_data$detailed_occupation_recode == 0 & test_data$weeks_worked_in_year == 0]) # [1] 5695
test_data$detailed_occupation_recode[test_data$detailed_occupation_recode == 0 & test_data$weeks_worked_in_year == 0] <- "Not in labor force"

length(test_data$major_industry_code[test_data$major_industry_code == "NIU" & test_data$weeks_worked_in_year == 0]) # [1] 5695
test_data$major_industry_code[test_data$major_industry_code == "NIU" & test_data$weeks_worked_in_year == 0] <- "Not in labor force"

length(test_data$major_occupation_code[test_data$major_occupation_code == "NIU" & test_data$weeks_worked_in_year == 0]) # [1] 5695
test_data$major_occupation_code[test_data$major_occupation_code == "NIU" & test_data$weeks_worked_in_year == 0] <- "Not in labor force"

# Remaining NIU in variables: 
length(test_data$major_occupation_code[test_data$major_occupation_code == "NIU"]) # [1] 1988
length(test_data$major_occupation_code[test_data$major_occupation_code == "NIU"])/nrow(test_data) # Prop: 0.01992743

######################################################################
##### RECODING: country_of_birth_father, country_of_birth_mother #####
######################################################################

# Assign NIU of father's country of origin to mother's, and viceversa.
# Assumption: 93% of parents are born in the same country:
sum(test_data$country_of_birth_father == test_data$country_of_birth_mother)/nrow(test_data) # [1] 0.9289208
# For country of father:
length(test_data$country_of_birth_father[test_data$country_of_birth_father == "NA" & test_data$country_of_birth_mother != "NA"]) # [1] 981
test_data$country_of_birth_father[test_data$country_of_birth_father == "NA" & test_data$country_of_birth_mother != "NA"] <- 
  test_data$country_of_birth_mother[test_data$country_of_birth_father == "NA" & test_data$country_of_birth_mother != "NA"]
# For country of mother:
length(test_data$country_of_birth_mother[test_data$country_of_birth_mother == "NA" & test_data$country_of_birth_father != "NA"]) # [1] 624
test_data$country_of_birth_mother[test_data$country_of_birth_mother == "NA" & test_data$country_of_birth_father != "NA"] <- 
  test_data$country_of_birth_father[test_data$country_of_birth_mother == "NA" & test_data$country_of_birth_father != "NA"]


# Convert categorical variables back to factors:
train_data  <- train_data %>%
  mutate_at(.vars = (names(train_data)[cat_vars_train]), funs(as.factor(.)))

test_data  <- test_data %>%
  mutate_at(.vars = (names(test_data)[cat_vars_test]), funs(as.factor(.)))

compare_factors <- compare(train_data, test_data)
compare_factors$vars.summary
#                                       var.x pos.x class.x                                     var.y pos.y class.y            values        attrs
# 10        detailed_household_and_family_stat    23  factor        detailed_household_and_family_stat    23  factor 92884 differences 1 attributes

##### SAVE DATASETS FOR DOWNSTREAM PROCESSES #####
saveRDS(train_data, file = "data/train_data_missing_recoded.RDS")
saveRDS(test_data, file = "data/test_data_missing_recoded.RDS")

##### HOUSECLEANING #####
rm(list=ls())