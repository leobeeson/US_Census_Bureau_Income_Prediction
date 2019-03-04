library(dplyr)
library(stringr)

##################################################
##### BUILDING THE TRAIN AND TEST DATAFRAMES #####
##################################################

##### READ IN TRAIN AND TEST SET #####
train_data <- read.csv(file = "data/census_income_learn.csv", header = FALSE)
test_data <- read.csv(file = "data/census_income_test.csv", header = FALSE)

##### READ IN CURATED METADATA #####
metadata_df <- readRDS(file = "resources/metadata_df.RDS")

##### ASSIGN VARIABLE NAMES TO TRAIN AND TEST SET #####
names(train_data) <- c(metadata_df$var_description, "income")
names(test_data) <- c(metadata_df$var_description, "income")
rm(metadata_df)

##### REMOVE COLUMNS NOT RELEVANT FOR EDA #####
# Instance Weight: 
# Criteria (from metadada file): "The instance weight indicates the number of people in the population that each record represents  
#                                 due to stratified sampling. To do real analysis and derive conclusions, this field must be used. 
#                                 This attribute should *not* be used in the classifiers, so it is set to "ignore" in this file."
train_data$instance_weight <- NULL    
test_data$instance_weight <- NULL

# Year:
# Criteria: Not enough years for making reliable time-series analysis.
train_data$year <- NULL    
test_data$year <- NULL

##### TRANSFORM VARIABLE TYPES #####
# detailed_industry_recode:
# Criteria: Not a ordinal/cardinal variable.
train_data$detailed_industry_recode <- as.factor(train_data$detailed_industry_recode)
test_data$detailed_industry_recode <- as.factor(test_data$detailed_industry_recode)

# detailed_occupation_recode:
# Criteria: Not a ordinal/cardinal variable.# Criteria: Not a cardinal variable.
train_data$detailed_occupation_recode <- as.factor(train_data$detailed_occupation_recode)
test_data$detailed_occupation_recode <- as.factor(test_data$detailed_occupation_recode)

# own_business_or_self_employed:
# Criteria: Not a ordinal/cardinal variable.
train_data$own_business_or_self_employed <- as.factor(train_data$own_business_or_self_employed)
test_data$own_business_or_self_employed <- as.factor(test_data$own_business_or_self_employed)

# veterans_benefits:
# Criteria: Not a ordinal/cardinal variable.
train_data$veterans_benefits <- as.factor(train_data$veterans_benefits)
test_data$veterans_benefits <- as.factor(test_data$veterans_benefits)

##### RECODE OUTCOME VARIABLES #####
# Criteria: More convenient for binary classification algorithms (and human/analyst readability).
train_data$income <- trimws(as.character(train_data$income)) %>%
  recode('- 50000.' = "0",  '50000+.' = "1") %>%
  as.factor()
test_data$income <- trimws(as.character(test_data$income)) %>%
  recode('- 50000.' = "0",  '50000+.' = "1") %>%
  as.factor()

##### RECODE NA / NIU VALUES #####
cat_vars_train <- unname(sapply(train_data, class)) == "factor"
train_data[,cat_vars_train] <- sapply(train_data[,cat_vars_train], as.character) %>%
  trimws() %>%
  str_replace_all(pattern = "Not in universe.*$", replacement = "NIU") %>%
  str_replace_all(pattern = "\\?", replacement = "NA") %>%
  str_replace_all(pattern = "Not identifiable", replacement = "NA")
train_data  <- train_data %>%
  mutate_at(.vars = (names(train_data)[cat_vars_train]), funs(as.factor(.)))

cat_vars_test <- unname(sapply(test_data, class)) == "factor"
test_data[,cat_vars_test] <- sapply(test_data[,cat_vars_test], as.character) %>%
  trimws() %>%
  str_replace_all(pattern = "Not in universe.*$", replacement = "NIU") %>%
  str_replace_all(pattern = "\\?", replacement = "NA") %>%
  str_replace_all(pattern = "Not identifiable", replacement = "NA") 
test_data  <- test_data %>%
  mutate_at(.vars = (names(test_data)[cat_vars_test]), funs(as.factor(.)))

##### SAVE DATASETS FOR DOWNSTREAM PROCESSES #####
saveRDS(train_data, file = "data/train_data.RDS")
saveRDS(test_data, file = "data/test_data.RDS")

##### HOUSE CLEANING #####
rm(train_data, test_data, cat_vars_train, cat_vars_test)
