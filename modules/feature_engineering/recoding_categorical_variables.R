library(dplyr)
library(arsenal)


##### READ IN TRAIN AND TEST SET #####
train_data <- readRDS(file = "data/train_data_missing_recoded.RDS")
test_data <- readRDS(file = "data/test_data_missing_recoded.RDS")

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

###############################
##### RECODING: education #####
###############################

# Reduce number of factors:
prop.table(table(train_data$education, train_data$income), 1)
edu_curr_levels <- c("High school graduate", "Some college but no degree", "10th grade", "Children", "Bachelors degree(BA AB BS)", 
                     "Masters degree(MA MS MEng MEd MSW MBA)", "Less than 1st grade", "Associates degree-academic program", "7th and 8th grade",
                     "12th grade no diploma", "Associates degree-occup /vocational", "Prof school degree (MD DDS DVM LLB JD)", "5th or 6th grade",
                     "11th grade", "Doctorate degree(PhD EdD)", "9th grade", "1st 2nd 3rd or 4th grade")
length(unique(edu_curr_levels)) # [1] 17

edu_new_levels <- c("School finished", "Undergraduate unfinished", "School unfinished", "Children", "Undergraduate finished", "Graduate", "No school",
                    "Undergraduate unfinished", "School unfinished", "School unfinished", "Undergraduate unfinished", "Post Graduate", "School unfinished",
                    "School unfinished", "Post Graduate", "School unfinished", "School unfinished")
length(unique(edu_new_levels)) # [1] 8

# Create a named vector for applying the replacements to the datasets:
names(edu_new_levels) <- edu_curr_levels

# Replace current factor levels with new ones:
train_data$education[] <- edu_new_levels[train_data$education]
test_data$education[] <- edu_new_levels[test_data$education]

###############################
##### RECODING: education #####
###############################

# Reduce number of factors: Join "Mexican (Mexicano)", "Mexican-American", and "Chicano", as they represent the same mexican origin.
mex_curr_levels <- c("Mexican (Mexicano)", "Mexican-American", "Chicano")
mex_new_level <- "Mexican"

sum(train_data$hispanic_origin %in% mex_curr_levels) # [1] 15617
sum(test_data$hispanic_origin %in% mex_curr_levels) # [1] 7866

train_data$hispanic_origin[train_data$hispanic_origin %in% mex_curr_levels] <- mex_new_level
test_data$hispanic_origin[test_data$hispanic_origin %in% mex_curr_levels] <- mex_new_level


##########################################################################
##### RECODING: capital_gains, capital_losses, dividends_from_stocks #####
##########################################################################

# capital_gains:
cap_gains_bins <- cut(train_data$capital_gains, breaks = seq(0, max(train_data$capital_gains) + 1, round(max(train_data$capital_gains)/10)), right = FALSE, dig.lab = 10)
table(cap_gains_bins, train_data$income)
prop.table(table(cap_gains_bins, train_data$income),1) # Probabilities of income > 50,000 reverse when capital_gains > 10,000:
threshold_cap_gains <- 10000
train_data$capital_gains <- ifelse(train_data$capital_gains < threshold_cap_gains, "0", "1")
test_data$capital_gains <- ifelse(test_data$capital_gains < threshold_cap_gains, "0", "1")

# capital_losses:
cap_loss_bins <- cut(train_data$capital_losses, breaks = c(0,1900,5000), right = FALSE, dig.lab = 10)
table(cap_loss_bins, train_data$income)
prop.table(table(cap_loss_bins, train_data$income),1) # Probabilities of income > 50,000 around 50% when capital_losses > 1,900:
threshold_cap_loss <- 1900
train_data$capital_losses <- ifelse(train_data$capital_losses < threshold_cap_loss, "0", "1")
test_data$capital_losses <- ifelse(test_data$capital_losses < threshold_cap_loss, "0", "1")

# dividends_from_stocks:
divs_stocks_bins <- cut(train_data$dividends_from_stocks, breaks = c(0, 15000, 100000), right = FALSE, dig.lab = 10) #breaks = seq(0, max(train_data$dividends_from_stocks) + 1, round(max(train_data$dividends_from_stocks)/100))
table(divs_stocks_bins, train_data$income)
prop.table(table(divs_stocks_bins, train_data$income),1) # Probabilities of income > 50,000 around 50% when dividends_from_stocks > 1,900:
threshold_divs_stocks <- 15000
train_data$dividends_from_stocks <- ifelse(train_data$dividends_from_stocks < threshold_divs_stocks, "0", "1")
test_data$dividends_from_stocks <- ifelse(test_data$dividends_from_stocks < threshold_divs_stocks, "0", "1")

# Convert categorical variables back to factors:
train_data  <- train_data %>%  
  mutate_at(.vars = (names(train_data)[unname(sapply(train_data, class)) == "character"]), funs(as.factor(.)))

test_data  <- test_data %>%
  mutate_at(.vars = (names(test_data)[unname(sapply(test_data, class)) == "character"]), funs(as.factor(.)))

compare_factors <- compare(train_data, test_data)
compare_factors$vars.summary
#                                       var.x pos.x class.x                                     var.y pos.y class.y            values        attrs
# 10        detailed_household_and_family_stat    23  factor        detailed_household_and_family_stat    23  factor 92884 differences 1 attributes

##### SAVE DATASETS FOR DOWNSTREAM PROCESSES #####
saveRDS(train_data, file = "data/train_data_factors_recoded.RDS")
saveRDS(test_data, file = "data/test_data_factors_recoded.RDS")

##### HOUSECLEANING #####
rm(list=ls())
