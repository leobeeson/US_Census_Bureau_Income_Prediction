library(dplyr)
library(VIM)
library(stringr)

##### READ IN TRAIN AND TEST SET #####
train_data <- readRDS(file = "data/train_data_factors_recoded.RDS")
test_data <- readRDS(file = "data/test_data_factors_recoded.RDS")

# Identify categorical values, excluding outcome variable:
cat_vars_train <- unname(sapply(train_data[,1:39], class)) == "factor"
cat_vars_test <- unname(sapply(test_data[,1:39], class)) == "factor"

# Convert remaining "NA" and "NIU" to NA:
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

##### JOIN TRAIN AND TEST SET #####
full_data <- rbind(train_data, test_data)

# Subset data with variables containing NA:
full_data_with_na <- full_data[colnames(full_data)[colSums(is.na(full_data)) > 0]]
rm(full_data)

##############################################
##### MAP MISSING VALUES IN FULL DATASET #####
##############################################

# Aggregate and plot missing values for variables containing NA: 
aggr_plot <- aggr(full_data_with_na)
ppi <- 300
png(filename = "outputs/feature_engineering/missing_data.png", width=10*ppi, height=6*ppi, res=ppi)
plot(aggr_plot, col=c('palegreen1','indianred1'), combined = FALSE, sortVars=TRUE, only.miss = FALSE, bars = FALSE, border = NA,
     labels=str_sub(names(full_data_with_na), start = 1, end = 10), cex.numbers= 0.5, cex.axis=0.7, gap=1, 
     ylab=c("Proportions of Missing Values in Training + Test Set ","Mosaic"))
dev.off()
rm(full_data_with_na, ppi)

# Choose variables to remove from dataset based on threshold of proportion of missing values:
threshold <- 0.05
missing_vars <- aggr_plot$missings
vars_to_remove <- missing_vars %>%
  mutate(props = Count / (nrow(train_data) + nrow(test_data))) %>%
  filter(props >threshold) %>%
  select(Variable) %>%
  pull()

##### REMOVE SELECTED VARIABLES FROM DATASET #####
train_data <- train_data[,!names(train_data) %in% vars_to_remove]
test_data <- test_data[,!names(test_data) %in% vars_to_remove]

saveRDS(train_data, file = "data/train_data_factors_na_removed.RDS")
saveRDS(test_data, file = "data/test_data_factors_na_removed.RDS")

##### REMOVE OBSERVATIONS WHICH STILL HAVE NAs #####
# complete_cases <- complete.cases(train_data)
# (nrow(train_data) - sum(complete_cases)) # [1] 10093
# train_data <- train_data[complete_cases, ]
# 
# complete_cases_test <- complete.cases(test_data)
# head(test_data[!complete_cases_test,], n = 20)


##########################################################
# ##### IMPUTE MISSING VALUES ON REMAINING VARIABLES #####
# ########################################################
t1 <- Sys.time()
train_data_imputed <- kNN(train_data)
t2 <- Sys.time()
t2 - t1 # Time difference of 1.676212 hours

sum(is.na(train_data_imputed))
saveRDS(train_data_imputed, file = "data/train_data_factors_na_imputed.RDS")


# library(mice)
# ########################################################
# ##### IMPUTE MISSING VALUES ON REMAINING VARIABLES #####
# ########################################################
# t1 <- Sys.time()
# temp_train_data <- mice(train_data, m = 5, maxit = 5, method = "pmm")
# t2 <- Sys.time()  ##### TAKING TOO LONG! SHORT-TERM TACTIC: REMOVE OBSERVATIONS WITH NAs.
# t2 - t1