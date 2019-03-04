library(caret)
library(gbm)
library(pROC)
library(e1071)
library(corrplot)
source("modules/model_building/optimising_classification_threshold.R")

##### READ IN TRAIN AND TEST SET WITH MISSING VALUES IMPUTATION #####
train_data <- readRDS(file = "data/train_data_for_model_building.RDS")   
test_data <- readRDS(file = "data/test_data_for_model_building.RDS")  


##### SPLIT TEST DATA INTO VALIDATION AND TEST DATA #####
val_test_index <- createDataPartition(y = test_data$income, p = .66, list = FALSE, times = 1)
validation_data <- test_data[val_test_index, ]
test_data <- test_data[-val_test_index, ]

##### DEFINE TRAINING PARAMETERS ##### 
control_train <- trainControl(sampling = "smote", "repeatedcv", number=10, repeats=3, classProbs = TRUE, savePredictions = "final", returnData = FALSE, trim = TRUE)

##### PARALLELISE IF AVAILABLE #####
if(require("doParallel")){
  registerDoParallel(cores = parallel::detectCores() - 1)
  paste0("Number of cores assigned for parallelisation: ",getDoParWorkers())
}

##############################################
##### MODEL 1: GRADIENT BOOSTING MACHINE #####
##############################################
model_name <- "Gradient Boosting Machine - Base Layer"

##### SET HYPERPARAMETER TUNING GRID #####
gbm_grid <- expand.grid(interaction.depth = c(5, 10, 15),
                        n.trees = c(100, 150, 200),
                        shrinkage = c(0.15, 0.1, 0.05),
                        n.minobsinnode = 10)

##### TRAIN MODEL #####
#t1 <- Sys.time()
#model_gbm <- train(income~., data=train_data, method="gbm", trControl=control_train, tuneGrid = gbm_grid, metric = "Kappa", keep.data = FALSE)
#t2 <- Sys.time() # Time difference of 3.454841 hours
#t2 - t1
#saveRDS(model_gbm, file = "outputs/model_building/model_gbm.RDS")
model_gbm <- readRDS(file = "outputs/model_building/model_gbm.RDS")

##### PREDICT ON VALIDATION SET #####
# Analyse default classification results (probability threshold = 0.5):
validation_data$pred_gbm_class_raw <- predict(object = model_gbm, newdata = validation_data, type = "raw")
confusionMatrix(validation_data$pred_gbm_class_raw, validation_data$income, positive = "more")
print(postResample(pred = validation_data$pred_gbm_class_raw, obs = validation_data$income)) # 0.4516129 0.0804837 
### Very poor performance. Only 8% higher than random prediction-reference classification agreement.

##### PROBABILITY THRESHOLD OPTIMISATION #####

# Get prediction probabilities for income == "more":
validation_data$pred_gbm_prob_more <- predict(object = model_gbm, newdata = validation_data, type = "prob")$more

# Optimize classification probability threshold:
t1 <- Sys.time()
threshold_gbm <- optimize_threshold(labels = validation_data$income, probs = validation_data$pred_gbm_prob_more)
t2 <- Sys.time(); t2 - t1 # Time difference of 4.119202 mins

# Select as threshold p where Kappa is max:
class_threshold <- threshold_gbm$Threshold[threshold_gbm$Kappa == max(threshold_gbm$Kappa)][1]

# Visualise relationship between Kappa vs. probability threshold:
ggplot(data=threshold_gbm, aes(x=Threshold, y=Kappa)) +
  geom_line(color="tomato1") + 
  geom_hline(aes(yintercept=max(Kappa)), color="seagreen2")+
  geom_text(aes(x=Threshold[Kappa == max(Kappa)][1], 
                label=paste("Kappa: ",round(max(Kappa), 2),"\np: ",Threshold[Kappa == max(Kappa)][1]), y=max(Kappa)), 
            size = 3, hjust = 1.5, vjust = 1.5, show.legend = FALSE,
            color="firebrick4")+
  labs(title = "Kappa vs. Probability Classification Threshold",
       subtitle = model_name) 

##### PREDICT ON VALIDATION SET USING OPTIMISED PROBABILITY THRESHOLD #####
validation_data$pred_gbm_class_opt <- ifelse(validation_data$pred_gbm_prob_more >= class_threshold, "more", "less")

# Compare prior probabilities to prediction probabilities:
priors_validation_data <- estimte_prior_probs(labels = validation_data$income) # [1] 0.06201021
post_validation_data <- sum(validation_data$pred_gbm_class_opt == "more")/nrow(validation_data) # [1] 0.08564182
paste0("Prior probs: ",round(priors_validation_data, 4), "\nPrediction probs: ", round(post_validation_data, 4))

##### PREDICT ON TEST SET USING OPTIMISED PROBABILITY THRESHOLD #####
test_data$pred_gbm_prob_more <- predict(object = model_gbm, newdata = test_data, type = "prob")$more
test_data$pred_gbm_class_opt <- ifelse(test_data$pred_gbm_prob_more >= class_threshold, "more", "less")

# Compare prior probabilities to prediction probabilities:
priors_test_data <- estimte_prior_probs(labels = test_data$income) # [1] 0.06200248
post_test_data <- sum(test_data$pred_gbm_class_opt == "more")/nrow(test_data) # [1] 0.08721033
paste0("Prior probs: ",round(priors_validation_data, 4), "\nPrediction probs: ", round(post_test_data, 4))

##### GOOD HOUSEKEEPING #####
rm(list=setdiff(ls(), c("train_data", "test_data", "validation_data", "control_train", 
                        "estimte_prior_probs", "classify_predictions", "optimize_threshold")))
ls()


################################################################
##### MODEL 2: RECURSIVE PARTINIONING AND REGRESSION TREES #####
################################################################
model_name <- "Recursive Partioning and Regression Trees - Base Layer"

##### TRAIN MODEL #####
# t1 <- Sys.time()
# model_rpart <- train(income~., data=train_data, method="rpart", trControl=control_train, tuneLength = 10)
# t2 <- Sys.time() 
# t2 - t1 # Time difference of 16.9226 mins
#saveRDS(model_rpart, file = "outputs/model_building/model_rpart.RDS")
model_rpart <- readRDS(file = "outputs/model_building/model_rpart.RDS")

##### PREDICT ON VALIDATION SET #####
# Analyse default classification results (probability threshold = 0.5):
validation_data$pred_rpart_class_raw <- predict(object = model_rpart, newdata = validation_data, type = "raw")
confusionMatrix(validation_data$pred_rpart_class_raw, validation_data$income, positive = "more")
print(postResample(pred = validation_data$pred_rpart_class_raw, obs = validation_data$income)) # 0.7904896 0.2562912
### Significantly better performance than GBM. A 26% higher than random prediction-reference classification agreement.

##### PROBABILITY THRESHOLD OPTIMISATION #####

# Get prediction probabilities for income == "more":
validation_data$pred_rpart_prob_more <- predict(object = model_rpart, newdata = validation_data, type = "prob")$more

# Optimize classification probability threshold:
t1 <- Sys.time()
threshold_rpart <- optimize_threshold(labels = validation_data$income, probs = validation_data$pred_rpart_prob_more)
t2 <- Sys.time(); t2 - t1 # Time difference of 4.119202 mins

# Select as threshold p where Kappa is max:
class_threshold <- threshold_rpart$Threshold[threshold_rpart$Kappa == max(threshold_rpart$Kappa)][1]

# Visualise relationship between Kappa vs. probability threshold:
ggplot(data=threshold_rpart, aes(x=Threshold, y=Kappa)) +
  geom_line(color="tomato1") + 
  geom_hline(aes(yintercept=max(Kappa)), color="seagreen2")+
  geom_text(aes(x=Threshold[Kappa == max(Kappa)][1], 
                label=paste("Kappa: ",round(max(Kappa), 2),"\np: ",Threshold[Kappa == max(Kappa)][1]), y=max(Kappa)), 
            size = 3, hjust = 1.5, vjust = 1.5, show.legend = FALSE,
            color="firebrick4")+
  labs(title = "Kappa vs. Probability Classification Threshold",
       subtitle = model_name) 

# Analyse summary statistics predictionpredicted probabilities for class "more" by rpart model:
summary(validation_data$pred_rpart_prob_more)

##### PREDICT ON VALIDATION SET USING OPTIMISED PROBABILITY THRESHOLD #####
validation_data$pred_rpart_class_opt <- ifelse(validation_data$pred_rpart_prob_more >= class_threshold, "more", "less")

# Compare prior probabilities to prediction probabilities:
priors_validation_data <- estimte_prior_probs(labels = validation_data$income) # [1] 0.06201021
post_validation_data <- sum(validation_data$pred_rpart_class_opt == "more")/nrow(validation_data) # [1] 0.1499909
paste0("Prior probs: ",round(priors_validation_data, 4), "\nPrediction probs: ", round(post_validation_data, 4))

##### PREDICT ON TEST SET USING OPTIMISED PROBABILITY THRESHOLD #####
test_data$pred_rpart_prob_more <- predict(object = model_rpart, newdata = test_data, type = "prob")$more
test_data$pred_rpart_class_opt <- ifelse(test_data$pred_rpart_prob_more >= class_threshold, "more", "less")

# Compare prior probabilities to prediction probabilities:
priors_test_data <- estimte_prior_probs(labels = test_data$income) # [1] 0.06200248
post_test_data <- sum(test_data$pred_rpart_class_opt == "more")/nrow(test_data) # [1] 0.1514535
paste0("Prior probs: ",round(priors_validation_data, 4), "\nPrediction probs: ", round(post_test_data, 4))

##### GOOD HOUSEKEEPING #####
rm(list=setdiff(ls(), c("train_data", "test_data", "validation_data", "control_train", 
                        "estimte_prior_probs", "classify_predictions", "optimize_threshold")))
ls()

################################
##### MODEL 3: BAGGED CART #####
################################
model_name <- "Bagged Classification and Regression Trees - Base Layer"

##### TRAIN MODEL #####
# t1 <- Sys.time()
# model_cart <- train(income~., data=train_data, method="treebag", trControl=control_train, metric = "Kappa", keep.data = FALSE)
# t2 <- Sys.time() 
# t2 - t1 # Time difference of 29.44355 mins
#saveRDS(model_cart, file = "outputs/model_building/model_cart.RDS")
model_cart <- readRDS(file = "outputs/model_building/model_cart.RDS")

##### PREDICT ON VALIDATION SET #####
# Analyse default classification results (probability threshold = 0.5):
validation_data$pred_cart_class_raw <- predict(object = model_cart, newdata = validation_data, type = "raw")
confusionMatrix(validation_data$pred_cart_class_raw, validation_data$income, positive = "more")
print(postResample(pred = validation_data$pred_cart_class_raw, obs = validation_data$income)) # 0.52613754 0.09906794 
### Almost the same performance as GBM. Only 10% higher than random prediction-reference classification agreement.

##### PROBABILITY THRESHOLD OPTIMISATION #####

# Get prediction probabilities for income == "more":
validation_data$pred_cart_prob_more <- predict(object = model_cart, newdata = validation_data, type = "prob")$more

# Optimize classification probability threshold:
t1 <- Sys.time()
threshold_cart <- optimize_threshold(labels = validation_data$income, probs = validation_data$pred_cart_prob_more)
t2 <- Sys.time(); t2 - t1 # Time difference of 6.164637 mins

# Select as threshold p where Kappa is max:
class_threshold <- threshold_cart$Threshold[threshold_cart$Kappa == max(threshold_cart$Kappa)][1]

# Visualise relationship between Kappa vs. probability threshold:
ggplot(data=threshold_cart, aes(x=Threshold, y=Kappa)) +
  geom_line(color="tomato1") + 
  geom_hline(aes(yintercept=max(Kappa)), color="seagreen2")+
  geom_text(aes(x=Threshold[Kappa == max(Kappa)][1], 
                label=paste("Kappa: ",round(max(Kappa), 2),"\np: ",Threshold[Kappa == max(Kappa)][1]), y=max(Kappa)), 
            size = 3, hjust = 1.5, vjust = 1.5, show.legend = FALSE,
            color="firebrick4")+
  labs(title = "Kappa vs. Probability Classification Threshold",
       subtitle = model_name) 

# Analyse summary statistics predictionpredicted probabilities for class "more" by cart model:
summary(validation_data$pred_cart_prob_more)

##### PREDICT ON VALIDATION SET USING OPTIMISED PROBABILITY THRESHOLD #####
validation_data$pred_cart_class_opt <- ifelse(validation_data$pred_cart_prob_more >= class_threshold, "more", "less")

# Compare prior probabilities to prediction probabilities:
priors_validation_data <- estimte_prior_probs(labels = validation_data$income) # [1] 0.06201021
post_validation_data <- sum(validation_data$pred_cart_class_opt == "more")/nrow(validation_data) # [1] 0.1499301
paste0("Prior probs: ",round(priors_validation_data, 4), "\nPrediction probs: ", round(post_validation_data, 4))

##### PREDICT ON TEST SET USING OPTIMISED PROBABILITY THRESHOLD #####
test_data$pred_cart_prob_more <- predict(object = model_cart, newdata = test_data, type = "prob")$more
test_data$pred_cart_class_opt <- ifelse(test_data$pred_cart_prob_more >= class_threshold, "more", "less")

# Compare prior probabilities to prediction probabilities:
priors_test_data <- estimte_prior_probs(labels = test_data$income) # [1] 0.06200248
post_test_data <- sum(test_data$pred_cart_class_opt == "more")/nrow(test_data) # [1] 0.1526918
paste0("Prior probs: ",round(priors_validation_data, 4), "\nPrediction probs: ", round(post_test_data, 4))

##### GOOD HOUSEKEEPING #####
rm(list=setdiff(ls(), c("train_data", "test_data", "validation_data", "control_train", 
                        "estimte_prior_probs", "classify_predictions", "optimize_threshold")))
ls()

##############################
##### MODEL CORRELATIONS #####
##############################

prediction_probs_labels <- c("pred_gbm_prob_more", "pred_rpart_prob_more", "pred_cart_prob_more")
prediction_probs_validation_data <- validation_data[,names(validation_data) %in% prediction_probs_labels]
colnames(prediction_probs_validation_data) <- c("GBM", "RPART", "CART")
correlation_preds_probls_val <- cor(prediction_probs_validation_data)
corrplot.mixed(correlation_preds_probls_val, upper = "color")

############################
##### MODEL ENSEMBLING #####
############################
model_name <- "Ensamble Model of GBM, RPART, & Bagged CART"

# Averaging: use type="prob", average probs, then ifelse greater than threshold
# Majority voting: use ifelse + &
# Weighed average: use use type="prob" * weights, then ifelse greater than threshold

# Stacking:
prediction_class_labels <- c("pred_gbm_class_opt", "pred_rpart_class_opt", "pred_cart_class_opt")


##### SET HYPERPARAMETER TUNING GRID #####
gbm_grid <- expand.grid(interaction.depth = c(5, 10, 15),
                        n.trees = c(100, 150, 200),
                        shrinkage = c(0.15, 0.1, 0.05),
                        n.minobsinnode = 10)

##### TRAIN TOP LAYER OF ENSEMBLE MODEL #####
t1 <- Sys.time()
model_ensemble <- train(income~., data=validation_data[,c(prediction_class_labels, "income")], method="gbm", trControl=control_train, tuneGrid = gbm_grid, metric = "Kappa", keep.data = FALSE)
t2 <- Sys.time()
t2- t1 # Time difference of 10.98253 mins

##### PREDICT ON TEST SET #####
# Analyse default classification results (probability threshold = 0.5):
test_data$pred_ensemble_class_raw <- predict(object = model_ensemble, newdata = test_data, type = "raw")
confusionMatrix(test_data$pred_ensemble_class_raw, test_data$income, positive = "more")
print(postResample(pred = test_data$pred_ensemble_class_raw, obs = test_data$income)) # 0.4516129 0.0804837 
### Very good performance. 35% higher than random prediction-reference classification agreement.

##### PROBABILITY THRESHOLD OPTIMISATION #####

# Get prediction probabilities for income == "more":
test_data$pred_ensemble_prob_more <- predict(object = model_ensemble, newdata = test_data, type = "prob")$more

# Optimize classification probability threshold:
t1 <- Sys.time()
threshold_ensemble <- optimize_threshold(labels = test_data$income, probs = test_data$pred_ensemble_prob_more)
t2 <- Sys.time(); t2 - t1 # Time difference of 2.033221 mins

# Select as threshold p where Kappa is max:
class_threshold <- threshold_ensemble$Threshold[threshold_ensemble$Kappa == max(threshold_ensemble$Kappa)][1]

# Visualise relationship between Kappa vs. probability threshold:
ggplot(data=threshold_ensemble, aes(x=Threshold, y=Kappa)) +
  geom_line(color="tomato1") + 
  geom_hline(aes(yintercept=max(Kappa)), color="seagreen2")+
  geom_text(aes(x=Threshold[Kappa == max(Kappa)][1], 
                label=paste("Kappa: ",round(max(Kappa), 2),"\np: ",Threshold[Kappa == max(Kappa)][1]), y=max(Kappa)), 
            size = 3, hjust = 1.5, vjust = 1.5, show.legend = FALSE,
            color="firebrick4")+
  labs(title = "Kappa vs. Probability Classification Threshold",
       subtitle = model_name) 

##### PREDICT ON test SET USING OPTIMISED PROBABILITY THRESHOLD #####
test_data$pred_ensemble_class_opt <- as.factor(ifelse(test_data$pred_ensemble_prob_more >= class_threshold, "more", "less"))

# Compare prior probabilities to prediction probabilities:
priors_test_data <- estimte_prior_probs(labels = test_data$income) # [1] 0.06200248
post_test_data <- sum(test_data$pred_ensemble_class_opt == "more")/nrow(test_data) # [1] 0.04690725
paste0("Prior probs: ",round(priors_test_data, 4), "\nPrediction probs: ", round(post_test_data, 4))

##### EVALUATE ENSEMBLE MODEL PERFORMANCE #####
confusionMatrix(test_data$pred_ensemble_class_opt, test_data$income, positive = "more")
print(postResample(pred = test_data$pred_ensemble_class_opt, obs = test_data$income)) # 0.9367298 0.3862799  
### Even better performance. 39% higher than random prediction-reference classification agreement.



##### GOOD HOUSEKEEPING #####
rm(list=setdiff(ls(), c("train_data", "test_data", "test_data", "control_train", 
                        "estimte_prior_probs", "classify_predictions", "optimize_threshold")))
ls()
