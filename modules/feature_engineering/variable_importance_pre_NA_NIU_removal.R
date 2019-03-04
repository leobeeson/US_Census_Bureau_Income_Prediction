library(caret)
library(InformationValue)
library(gbm)
library(DMwR)

#################################################################################
##### VARIABLE IMPORTANCE OF FULL DATASET WITHOUT MISSING VALUES IMPUTATION #####
#################################################################################

##### READ IN TRAIN AND TEST SET WITHOUT MISSING VALUES IMPUTATION #####
train_data <- readRDS(file = "data/train_data_factors_recoded.RDS")
str(train_data)

# Change outcome variable factor levels:
levels(train_data$income) <- c("less", "more")

#############################################################
##### APPLY WEIGHT OF EVIDENCE TO CATEGORICAL VARIABLES #####
#############################################################

# Identify categorical values, excluding outcome variable:
cat_vars_train <- unname(sapply(train_data[,1:39], class)) == "factor"

# Calculate WOE for all categorical variables:
train_data[,cat_vars_train] <- sapply(train_data[,cat_vars_train], function(x) WOE(x,train_data$income, valueOfGood = 1))

########################################
##### ESTIMATE VARIABLE IMPORTANCE #####
########################################

# Set control parameters for model training:
control <- trainControl(sampling = "smote", method="cv", number=5, classProbs = TRUE, summaryFunction = twoClassSummary)

# Train gbm model for calculating variable importance:
t1 <- Sys.time()
model <- train(income~., data=train_data, method="gbm", trControl=control, metric = "ROC")
t2 <- Sys.time()
t2 - t1 #Time difference of 32.52398 mins
#saveRDS(model, file = "outputs/feature_engineering/model_gbm.RDS")
# model <- readRDS(file = "outputs/feature_engineering/model_gbm.RDS")

# Estimate variable importance:
var_importance <- varImp(model, scale=FALSE)
saveRDS(object = var_importance, file = "outputs/feature_engineering/var_importance_gbm.RDS")

# Summarize importance:
print(var_importance_woe)

# Plot and save variable importance:
ppi <- 300
png(filename = "outputs/feature_engineering/var_importance_ver_0.png", width=8*ppi, height=6*ppi, res=ppi)
plot(var_importance, main = "Variable Importance Estimated with GBM\n(method='repeatedcv', number=10, repeats=3")
dev.off()

##################################################
##### ESTIMATE CORRELATION BETWEEN VARIABLES #####
##################################################

# Calculate correlations between variables:
corr_matrix <- cor(train_data[,1:39])
highly_correlated <- findCorrelation(corr_matrix, cutoff=0.75, names = TRUE, verbose = TRUE)#, exact = TRUE)
print(highly_correlated)

