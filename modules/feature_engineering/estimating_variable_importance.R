library(caret)
library(InformationValue)
library(gbm)
library(DMwR)
library(dplyr)

#########################################################################
##### VARIABLE IMPORTANCE OF DATASET WITH MISSING VALUES IMPUTATION #####
#########################################################################

##### READ IN TRAIN AND TEST SET WITH MISSING VALUES IMPUTATION #####
train_data <- readRDS(file = "data/train_data_factors_na_removed.RDS")
#train_data$set <- NULL
test_data <- readRDS(file = "data/test_data_factors_na_removed.RDS")
#test_data$set <- NULL

# Identify categorical values, excluding outcome variable:
cat_vars_train <- unname(sapply(train_data[,1:ncol(train_data) - 1], class)) == "factor"
cat_vars_test <- unname(sapply(test_data[,1:ncol(test_data) - 1], class)) == "factor"
#all(cat_vars_train == cat_vars_test)

train_data[,cat_vars_train] <- sapply(train_data[,cat_vars_train], as.character)
train_data[is.na(train_data)] <- "NA"
train_data  <- train_data %>%
  mutate_at(.vars = (names(train_data)[cat_vars_train]), funs(as.factor(.)))

test_data[,cat_vars_test] <- sapply(test_data[,cat_vars_test], as.character)
test_data[is.na(test_data)] <- "NA"
test_data  <- test_data %>%
  mutate_at(.vars = (names(test_data)[cat_vars_test]), funs(as.factor(.)))

#############################################################
##### APPLY WEIGHT OF EVIDENCE TO CATEGORICAL VARIABLES #####
#############################################################

# Calculate WOE for all categorical variables:
train_data[,cat_vars_train] <- sapply(train_data[,cat_vars_train], function(x) WOE(x, train_data$income, valueOfGood = 1))
test_data[,cat_vars_test] <- sapply(test_data[,cat_vars_test], function(x) WOE(x, test_data$income, valueOfGood = 1))

######################################################
##### ESTIMATE VARIABLE IMPORTANCE WITH WOE DATA #####
######################################################

# Change outcome variable factor levels:
levels(train_data$income) <- c("less", "more")
levels(test_data$income) <- c("less", "more")

# Set control parameters for model training:
control <- trainControl(sampling = "smote", method="cv", number=5, classProbs = TRUE, summaryFunction = twoClassSummary)

# Train gbm model for calculating variable importance:
t1 <- Sys.time()
model <- train(income~., data=train_data, method="gbm", trControl=control, metric = "ROC")
t2 <- Sys.time() 
t2 - t1 # Time difference of 25.82817 mins

#saveRDS(model, file = "outputs/feature_engineering/model_gbm_WOE_VER1.RDS")

# Estimate variable importance:
var_importance_woe <- varImp(model, scale=FALSE)
saveRDS(object = var_importance_woe, file = "outputs/feature_engineering/var_importance_gbm_woe.RDS")

# Summarize importance:
print(var_importance_woe)

# Plot and save variable importance:
ppi <- 300
png(filename = "outputs/feature_engineering/var_importance_ver_0.png", width=8*ppi, height=6*ppi, res=ppi)
plot(var_importance_woe, main = "Variable Importance Estimated with GBM\n(method='cv', number=5")
dev.off()

##################################################
##### ESTIMATE CORRELATION BETWEEN VARIABLES #####
##################################################

# Calculate correlations between variables:
corr_matrix <- cor(train_data[,1:ncol(train_data) - 1])
highly_correlated <- findCorrelation(corr_matrix, cutoff=0.75, names = TRUE, verbose = TRUE)
# RESULTS:
# Compare row 2  and column  26 with corr  0.925 
# Means:  0.39 vs 0.197 so flagging column 2        Francis: ACCEPT
# Compare row 26  and column  19 with corr  0.831 
# Means:  0.368 vs 0.182 so flagging column 26      Francis: ACCEPT
# Compare row 19  and column  18 with corr  0.973 
# Means:  0.333 vs 0.168 so flagging column 19      Francis: REVERSE -> var 19 (detailed_household_summary_in_household) among top-5 vars from variable importance analysis. Remove var 18: detailed_household_and_family_stat
# Compare row 4  and column  8 with corr  0.849 
# Means:  0.272 vs 0.155 so flagging column 4       Francis: REVERSE -> var 4 among top-5 vars from variable importance analysis. Var 8 gets flagged on the next line. Remove var 9: major_occupation_code
# Compare row 8  and column  3 with corr  0.992 
# Means:  0.242 vs 0.146 so flagging column 8       Francis: ACCEPT 
# Compare row 3  and column  9 with corr  0.856 
# Means:  0.207 vs 0.138 so flagging column 3       Francis: ACCEPT 
# Compare row 21  and column  22 with corr  0.87 
# Means:  0.166 vs 0.133 so flagging column 21      Francis: ACCEPT 
# All correlations <= 0.75 
print(highly_correlated)

#################################################################################
##### REMOVE CORRELATED VARIABLES NOT AMONG TOP-N MOST IMPORTANT VARIABLES #####
################################################################################

# Choose n of most important variables:
n <- 5
top_n_important_vars <- rownames(var_importance_woe$importance)[1:n]

# Curate variables to remove:
vars_to_remove <- highly_correlated
vars_to_remove <- vars_to_remove[!vars_to_remove %in% top_n_important_vars] 
vars_to_remove <- c(vars_to_remove, "detailed_household_and_family_stat", "major_occupation_code")

train_data <- train_data[,! names(train_data) %in% vars_to_remove]
test_data <- test_data[,! names(test_data) %in% vars_to_remove]

saveRDS(train_data, file = "data/train_data_for_model_building.RDS")
saveRDS(test_data, file = "data/test_data_for_model_building.RDS")
