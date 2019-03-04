library(dplyr)
library(skimr)

##### READ IN TRAIN AND TEST SET #####
train_data <- readRDS(file = "data/train_data.RDS")
test_data <- readRDS(file = "data/test_data.RDS")

####################################################################
##### PRODUCE GLOBAL SUMMARY STATISTICS FOR TEST AND TRAIN SET #####
####################################################################

# Identify categorical values:
cat_vars_train <- unname(sapply(train_data, class)) == "factor"
cat_vars_test <- unname(sapply(test_data, class)) == "factor"

# For the global summary statistics, assume NIU is also NA:
train_data[,cat_vars_train] <- sapply(train_data[,cat_vars_train], as.character)
train_data[train_data == "NA"] <- NA
train_data[train_data == "NIU"] <- NA
train_data  <- train_data %>%
  mutate_at(.vars = (names(train_data)[cat_vars_train]), funs(as.factor(.)))

test_data[,cat_vars_test] <- sapply(test_data[,cat_vars_test], as.character)
test_data[test_data == "NA"] <- NA
test_data[test_data == "NIU"] <- NA
test_data  <- test_data %>%
  mutate_at(.vars = (names(test_data)[cat_vars_test]), funs(as.factor(.)))

# Get global statistics:
skim_train_data <- skim(train_data)
kable(skim_train_data, format = " html")

view(dfSummary(train_data))
view(dfSummary(test_data))

##### HOUSE CLEANING #####
rm(train_data, test_data, cat_vars_train, cat_vars_test, skim_train_data)
