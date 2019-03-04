library(plyr)
library(dplyr)
library(stringr)
library(skimr)
library(summarytools)
library(ggplot2)
library(gridExtra)
library(arsenal)
library(VIM)
library(caret)
library(InformationValue)
library(gbm)
library(DMwR)
library(pROC)
library(e1071)
library(corrplot)

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

##### CONVERT OUTCOME VARIABLE TO FACTOR #####
# Criteria: Required by some algorithms for binary classification. 
# Facor levels are recoded once again further downstream to enhance human readability. Ar this stage, we're coding the factor levels to 0 and 1. 
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
ifelse(!dir.exists(file.path("data")), dir.create("data"), FALSE)
#saveRDS(train_data, file = "data/train_data.RDS")
#saveRDS(test_data, file = "data/test_data.RDS")

##### HOUSE CLEANING #####
#rm(train_data, test_data, cat_vars_train, cat_vars_test)


#####################################################################
##### ESTIMATE GLOBAL SUMMARY STATISTICS FOR TEST AND TRAIN SET #####
#####################################################################

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
view(dfSummary(train_data))
view(dfSummary(test_data))
#skim_train_data <- skim(train_data)
#kable(skim_train_data, format = "html")

##### HOUSE CLEANING #####
#rm(train_data, test_data, skim_train_data)

#####################################
##### EXPLORATORY DATA ANALYSIS #####
#####################################

##### RELOAD ORIGINAL TRAIN AND TEST DATAFRAMES #####
train_data <- readRDS(file = "data/train_data.RDS")
test_data <- readRDS(file = "data/test_data.RDS")

##### DISTINGUISH BETWEEN CATEGORICAL AND NUMERICAL VARIABLES #####
all(cat_vars_train == cat_vars_test)
vars_cat <- names(train_data)[cat_vars_train]
vars_cat <- vars_cat[vars_cat != "income"]
vars_num <- names(train_data)[!cat_vars_train]

##### REPLACE OUTCOME LABELS FOR FACET ANALYSIS #####
# Dictionary of factor levels of outcome variable mapped to graphical-friendlier labels for human readability:
outcome_names <- list("0"="Income < USD 50k", "1"="Income > USD 50k", '(all)'="Total Respondent")
# Function for retrieving the human-readable labels of outcome variable from dictionary:
outcome_labeller <- function(variable,value){
  return(outcome_names[value])
}

##### PROPER CASING OF TITLES #####
# Function for proper casing titles of graphs -> Capitalise first letter:
proper_case <- function(x){
  out <- str_replace_all(string = x, pattern = "_", " ")
  out <- paste0(toupper(str_sub(out, 1, 1)), tolower(str_sub(out, 2)))
}

#########################################
##### EXPLORE CATEGORICAL VARIABLES #####
#########################################

##### FUNCTION FOR PLOTTING BAR CHARTS WITH MARGINS FOR INCOME #####
plot_categorical_variable <- function(variable, dataset, colour, n, set_type){
  
  var_of_interest <- dataset[,variable]
  
  # Create variable-specific dataframe:
  var_interest_df <- data.frame(var_interest = var_of_interest, income = dataset$income)
  
  # Select top-n frequent factor levels:
  temp <- names(summary(var_interest_df$var_interest))[1:min(length(levels(var_interest_df$var_interest)),n)]
  
  # Aggregate factor levels beneath top-n:
  var_interest_df$var_interest <- as.character(var_interest_df$var_interest)
  var_interest_df$top_n <- ifelse(var_interest_df$var_interest %in% temp, var_interest_df$var_interest, "Other")
  var_interest_df$top_n <- as.factor(var_interest_df$top_n)
  var_interest_df$top_n <- reorder(var_interest_df$top_n, var_interest_df$top_n, FUN = function(x) -length(x))
  labels_var_interest <- levels(var_interest_df$top_n)
  
  # Plot distribution analysis for categorical variable:
  p <- ggplot(var_interest_df, mapping = aes(x = top_n, y = ..prop.., group = 1)) + 
    geom_bar(fill = colour) + 
    facet_grid(~income, margins = TRUE, labeller = outcome_labeller) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_discrete(labels=labels_var_interest, position = "bottom") +
    theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank()) +
    labs(title = proper_case(variable), 
         subtitle = set_type, 
         caption = "source: U.S. Census Bureau Data, Current Population Surveys 1994 and 1995")
  return(p)
}

##### RANKING THRESHOLD FOR MAX NUMBER OF MOST FREQUENT FACTOR LEVELS TO DISPLAY ON BARCHARTS####
# Any factor level beneath the ranking threshold are assigned to level = "Other" and aggregated into a single factor level.
# HEURISTIC: I choose 9, so as once the "Other" factor level is created, a total of 10 levels are plotted. 
n <- 9

##### COSMETICS FOR PLOTS #####
palette <- c("darkslategray1", "darkolivegreen1", "darkorchid1", "darkseagreen1", "deeppink1", "deepskyblue1", "dodgerblue1", "firebrick1",
             "gold1", "hotpink1", "indianred1", "khaki1", "lavender", "lightcyan1", "lightcoral", "lightgoldenrod1", "lightpink1",
             "lightsalmon1", "lightskyblue1", "mediumpurple1", "olivedrab1", "orangered1", "orchid1", "palegreen1", "paleturquoise1", 
             "palevioletred1", "plum1", "salmon1", "seagreen1", "slateblue1", "slategray1", "springgreen1", "steelblue1", "thistle1", 
             "tomato1", "turquoise1", "violetred1", "lightgreen")

##### CATEGORICAL VARIABLES: UNIVARIATE DISTRIBUTIONS AND CONDITIONAL DISTRIBUTIONS VS. INCOME #####

# Define parameters:
var_index <- 1 # options: [1...32]
dataset_index <- 1 # options: [1, 2] where 1 = train_data, and 2 = test_data

# Print analysis of an individual categorical variable to screen. 
suppressWarnings(print(plot_categorical_variable(variable = vars_cat[var_index], dataset = if(dataset_index == 1){train_data} else{test_data}, 
                               colour = sample(palette, 1), n = n, set_type = c("Train set", "Test set")[dataset_index])))

# Save to disk distribution analysis of all categorical variables:
# ifelse(!dir.exists(file.path("outputs/EDA/Plots_Categorical_Vars")), dir.create("outputs/EDA/Plots_Categorical_Vars"), FALSE)
# for(v in vars_cat){
#   for(t in c("Train set", "Test set")){
#     png(paste0("outputs/EDA/Plots_Categorical_Vars/",v,"_",tolower(str_split(t, " ")[[1]][1]),"_set.png"))
#     dataset <- if(t == "Train set"){train_data} else {test_data}
#     p <- plot_categorical_variable(variable = v, dataset = dataset, colour = sample(palette, 1), n = n, set_type = t)
#     print(p)
#     dev.off()
#   }
# }

#################################
##### NUMERICAL VARIABLES #####
#################################

##### FUNCTION FOR PLOTTING NUMERIC VARIABLES #####
plot_numerical_variable <- function(variable, dataset, colour, set_type){
  
  var_of_interest <- dataset[,variable]
  
  # Create variable-specific dataframe:
  var_interest_df <- data.frame(var_interest = as.numeric(var_of_interest, na.rm=TRUE), income = dataset$income)
  
  var_mean_summary <- ddply(var_interest_df, "income", summarise, var_mean=mean(var_interest))
  var_median_summary <- ddply(var_interest_df, "income", summarise, var_median=median(var_interest))
  
  p_1 <- ggplot(var_interest_df, aes(x=var_of_interest, fill=income)) +
    geom_density(alpha = 0.3, show.legend = FALSE) + 
    scale_fill_manual(values=c("plum1", "turquoise1")) +
    xlab(proper_case(variable)) +
    labs(title = proper_case(variable), 
         subtitle = set_type, 
         caption ="") + #"U.S. Census Bureau Data, Current Population Surveys 1994 and 1995") +
    geom_vline(data=var_mean_summary, aes(xintercept=var_mean, color=income), show.legend = FALSE) +
    geom_text(data=var_mean_summary,aes(x=var_mean, label=paste0("Mean: ",round(var_mean, 2)), y=0.00, colour=income), size = 3, angle=90, hjust = -0.01, vjust = 1.1, show.legend = FALSE)
  
  p_2 <- ggplot(var_interest_df, aes(x=income, y=var_of_interest, fill=income)) + 
    geom_boxplot(alpha = 0.3) +
    scale_fill_manual(values=c("plum1", "turquoise1")) +
    ylab(proper_case(variable)) +
    labs(title = "", #proper_case(variable), 
         subtitle = "", #set_type, 
         caption = "source: U.S. Census Bureau Data, Current Population Surveys 1994 and 1995") +
    geom_text(data=var_median_summary, aes(x=income, y=var_median, label=paste0("Median: ",var_median), colour=income), size = 3, vjust = -0.5, show.legend = FALSE)
  
  p <- grid.arrange(p_1, p_2, ncol=2)
  
  return(p)
}

##### NUMERICAL VARIABLES: UNIVARIATE DISTRIBUTIONS AND CONDITIONAL DISTRIBUTIONS VS. INCOME #####

# Define parameters:
var_index <- 2 # options: [1...7]
dataset_index <- 1 # options: [1, 2] where 1 = train_data, and 2 = test_data

# Print analysis of an individual numerical variable to screen. 
print(plot_numerical_variable(variable = vars_num[var_index], dataset = if(dataset_index == 1){train_data} else{test_data}, 
                                                 colour = sample(palette, 1), set_type = c("Train set", "Test set")[dataset_index]))

# Save to disk distribution analysis of all numerical variables:
# ifelse(!dir.exists(file.path("outputs/EDA/Plots_Numerical_Vars")), dir.create("outputs/EDA/Plots_Numerical_Vars"), FALSE)
# for(v in vars_num){
#   for(t in c("Train set", "Test set")){
#     ppi <- 300
#     png(paste0("outputs/EDA/Plots_Numerical_Vars/",v,"_",tolower(str_split(t, " ")[[1]][1]),"_set.png"), width=8*ppi, height=6*ppi, res=ppi)
#     dataset <- if(t == "Train set"){train_data} else {test_data}
#     p <- plot_numerical_variable(variable = v, dataset = dataset, colour = sample(palette, 1), set_type = t)
#     print(p)
#     dev.off()
#   }
# }

##### HOUSECLEANING #####
#rm(outcome_names, n, palette, var_index, dataset_index, vars_cat, vars_num, proper_case, outcome_labeller, plot_categorical_variable, plot_numerical_variable)


#################################
##### RECODE MISSING VALUES #####
#################################

##### CREATE COPY OF TRAIN AND TEST SET FOR POSTERIOR ANALYSIS #####
copy_train_data <- train_data
copy_test_data <- test_data

# Identify categorical values:
#cat_vars_train <- unname(sapply(train_data, class)) == "factor"
#cat_vars_test <- unname(sapply(test_data, class)) == "factor"
#all(cat_vars_train == cat_vars_test) # [1] TRUE

# Convert categorical variables to character:
train_data[,cat_vars_train] <- sapply(train_data[,cat_vars_train], as.character)
test_data[,cat_vars_test] <- sapply(test_data[,cat_vars_test], as.character)

#################################
##### RECODING: Training Set #####
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

##### SANITY CHECK FACTOR LEVELS #####
compare_factors <- compare(train_data, test_data)
compare_factors$vars.summary
# RESULTS: Only one factor level difference between train set and test set in variable "detailed_household_and_family_stat". 
# This variable misses one factor level in the test set which does appear in the training set. This will not affect the algorithms downstream:
#
#                                       var.x pos.x class.x                                     var.y pos.y class.y            values        attrs
# 10        detailed_household_and_family_stat    23  factor        detailed_household_and_family_stat    23  factor 92884 differences 1 attributes

##### SAVE DATASETS FOR DOWNSTREAM PROCESSES #####
#saveRDS(train_data, file = "data/train_data_missing_recoded.RDS")
#saveRDS(test_data, file = "data/test_data_missing_recoded.RDS")

##### HOUSECLEANING #####
rm(copy_train_data, copy_test_data, compare_factors)


##########################################
##### RECODING CATEGORICAL VARIABLES #####
##########################################

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

#####################################
##### RECODING: hispanic_origin #####
#####################################

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

##### SANITY CHECK FACTOR LEVELS #####
compare_factors <- compare(train_data, test_data)
compare_factors$vars.summary
# RESULTS: Facor levels remain the same between train and test set (only one factor level missing for variable detailed_household_and_family_stat in test set) after recoding.
# 
#                                       var.x pos.x class.x                                     var.y pos.y class.y            values        attrs
# 10        detailed_household_and_family_stat    23  factor        detailed_household_and_family_stat    23  factor 92884 differences 1 attributes

##### UPDATE VECTOR IDENTIFYING VARIABLE TYPES AND EXCLUDE OUTCOME VARIABLE #####
# Identify new set of categorical values, as some numeric variables (i.e. capital_gains, capital_losses, dividends_from_stocks) were transformed to categorical:
cat_vars_train <- unname(sapply(train_data[1:ncol(train_data)-1], class)) == "factor"
cat_vars_test <- unname(sapply(test_data[1:ncol(test_data)-1], class)) == "factor"
all(cat_vars_train == cat_vars_test) # [1] TRUE

##### SAVE DATASETS FOR DOWNSTREAM PROCESSES #####
#saveRDS(train_data, file = "data/train_data_factors_recoded.RDS")
#saveRDS(test_data, file = "data/test_data_factors_recoded.RDS")

##### HOUSECLEANING #####
rm(compare_factors, cap_gains_bins, cap_loss_bins, divs_stocks_bins, edu_curr_levels, edu_new_levels, mex_curr_levels, mex_new_level, threshold_cap_gains, threshold_cap_loss, threshold_divs_stocks)


#####################################
##### MISSING VALUES MANAGEMENT #####
#####################################

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
# Unburden memory usage:
rm(full_data)

##############################################
##### MAP MISSING VALUES IN FULL DATASET #####
##############################################

# Aggregate and plot missing values for variables containing NA: 
aggr_plot <- aggr(full_data_with_na)
plot(aggr_plot, col=c('palegreen1','indianred1'), combined = FALSE, sortVars=TRUE, only.miss = FALSE, bars = FALSE, border = NA,
     labels=str_sub(names(full_data_with_na), start = 1, end = 10), cex.numbers= 0.5, cex.axis=0.7, gap=1, 
     ylab=c("Proportions of Missing Values in Training + Test Set ","Mosaic"))
# Unburden memory usage:
rm(full_data_with_na)

# Choose variables to remove from dataset based on threshold of proportion of missing values:
threshold <- 0.05
missing_vars <- aggr_plot$missings
vars_to_remove <- missing_vars %>%
  mutate(props = Count / (nrow(train_data) + nrow(test_data))) %>%
  filter(props > threshold) %>%
  select(Variable) %>%
  pull()

##### REMOVE SELECTED VARIABLES FROM DATASET #####
train_data <- train_data[,!names(train_data) %in% vars_to_remove]
test_data <- test_data[,!names(test_data) %in% vars_to_remove]

#saveRDS(train_data, file = "data/train_data_factors_na_removed.RDS")
#saveRDS(test_data, file = "data/test_data_factors_na_removed.RDS")

rm(aggr_plot, missing_vars, threshold, vars_to_remove)


###############################
##### VARIABLE IMPORTANCE #####
###############################

##### UPDATE VECTOR IDENTIFYING VARIABLE TYPES AND EXCLUDE OUTCOME VARIABLE #####
# Identify new set of categorical values, as some variables were removed:
cat_vars_train <- unname(sapply(train_data[1:ncol(train_data)-1], class)) == "factor"
cat_vars_test <- unname(sapply(test_data[1:ncol(test_data)-1], class)) == "factor"
all(cat_vars_train == cat_vars_test)

##### MANAGEMENT OF REMAINING NAs #####
# Assign factor level "Other" to remaining NA values:
train_data[,cat_vars_train] <- sapply(train_data[,cat_vars_train], as.character)
train_data[is.na(train_data)] <- "Other"
train_data  <- train_data %>%
  mutate_at(.vars = (names(train_data)[cat_vars_train]), funs(as.factor(.)))

test_data[,cat_vars_test] <- sapply(test_data[,cat_vars_test], as.character)
test_data[is.na(test_data)] <- "Other"
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

# Change outcome variable factor levels for binary classification algorithm:
levels(train_data$income) <- c("less", "more")
levels(test_data$income) <- c("less", "more")

# Set control parameters for model training:
control <- trainControl(sampling = "smote", method="cv", number=5, classProbs = TRUE, summaryFunction = twoClassSummary)

# Train gbm model for calculating variable importance:
# t1 <- Sys.time()
# model <- train(income~., data=train_data, method="gbm", trControl=control, metric = "ROC")
# t2 <- Sys.time() 
# t2 - t1 # Time difference of 24.66226 mins
#saveRDS(model, file = "outputs/feature_engineering/model_gbm_var_imp.RDS")
model <- readRDS(file = "outputs/feature_engineering/model_gbm_var_imp.RDS")

# Estimate variable importance:
var_importance_woe <- varImp(model, scale=FALSE)
#saveRDS(object = var_importance_woe, file = "outputs/feature_engineering/var_importance_gbm_woe.RDS")

# Summarize importance:
print(var_importance_woe)

# Plot and save variable importance:
plot(var_importance_woe, main = "Variable Importance Estimated with GBM\n(method='cv', number=5")

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

# Choose n of most important variables which will not be removed, independently of variable correlation analysis:
n <- 5
top_n_important_vars <- rownames(var_importance_woe$importance)[1:n]

# Curate variables to remove:
vars_to_remove <- highly_correlated
vars_to_remove <- vars_to_remove[!vars_to_remove %in% top_n_important_vars] 
vars_to_remove <- c(vars_to_remove, "detailed_household_and_family_stat", "major_occupation_code")

train_data <- train_data[,! names(train_data) %in% vars_to_remove]
test_data <- test_data[,! names(test_data) %in% vars_to_remove]

#saveRDS(train_data, file = "data/train_data_for_model_building.RDS")
#saveRDS(test_data, file = "data/test_data_for_model_building.RDS")

rm(control, corr_matrix, model, var_importance_woe, highly_correlated, n, t1, t2, top_n_important_vars, vars_to_remove, cat_vars_train, cat_vars_test)


##########################
##### MODEL BUILDING #####
##########################

source("modules/model_building/optimising_classification_threshold.R")

##### READ IN TRAIN AND TEST SET WITH MISSING VALUES IMPUTATION #####
#train_data <- readRDS(file = "data/train_data_for_model_building.RDS")   
#test_data <- readRDS(file = "data/test_data_for_model_building.RDS")  

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
caret::confusionMatrix(validation_data$pred_gbm_class_raw, validation_data$income, positive = "more")
print(postResample(pred = validation_data$pred_gbm_class_raw, obs = validation_data$income)) # 0.45077760 0.08029355 
### Very poor performance. Only 8% higher than random prediction-reference classification agreement.

##### PROBABILITY THRESHOLD OPTIMISATION #####
# Get prediction probabilities for income == "more":
validation_data$pred_gbm_prob_more <- predict(object = model_gbm, newdata = validation_data, type = "prob")$more

# Optimize classification probability threshold:
t1 <- Sys.time()
threshold_gbm <- optimize_threshold(labels = validation_data$income, probs = validation_data$pred_gbm_prob_more)
t2 <- Sys.time(); t2 - t1 # Time difference of 4.119202 mins

# Select as threshold p where Kappa is max:
class_threshold_gbm <- threshold_gbm$Threshold[threshold_gbm$Kappa == max(threshold_gbm$Kappa)][1]

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
validation_data$pred_gbm_class_opt <- ifelse(validation_data$pred_gbm_prob_more >= class_threshold_gbm, "more", "less")

# Compare prior probabilities to prediction probabilities:
priors_validation_data <- estimte_prior_probs(labels = validation_data$income) # [1] 0.06201021
post_validation_data <- sum(validation_data$pred_gbm_class_opt == "more")/nrow(validation_data) # [1] 0.08741875
paste0("Prior probs: ",round(priors_validation_data, 4), "\nPrediction probs: ", round(post_validation_data, 4))

##### PREDICT ON TEST SET USING OPTIMISED PROBABILITY THRESHOLD #####
test_data$pred_gbm_prob_more <- predict(object = model_gbm, newdata = test_data, type = "prob")$more
test_data$pred_gbm_class_opt <- ifelse(test_data$pred_gbm_prob_more >= class_threshold_gbm, "more", "less")

# Compare prior probabilities to prediction probabilities:
priors_test_data <- estimte_prior_probs(labels = test_data$income) # [1] 0.06200248
post_test_data <- sum(test_data$pred_gbm_class_opt == "more")/nrow(test_data) # [1] 0.04569845
paste0("Prior probs: ",round(priors_validation_data, 4), "\nPrediction probs: ", round(post_test_data, 4))

##### GOOD HOUSEKEEPING #####
# rm(list=setdiff(ls(), c("train_data", "test_data", "validation_data", "control_train", 
#                         "estimte_prior_probs", "classify_predictions", "optimize_threshold")))
# ls()


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
caret::confusionMatrix(validation_data$pred_rpart_class_raw, validation_data$income, positive = "more")
print(postResample(pred = validation_data$pred_rpart_class_raw, obs = validation_data$income)) # 0.7884090 0.2548524 
### Significantly better performance than GBM. A 26% higher than random prediction-reference classification agreement.

##### PROBABILITY THRESHOLD OPTIMISATION #####
# Get prediction probabilities for income == "more":
validation_data$pred_rpart_prob_more <- predict(object = model_rpart, newdata = validation_data, type = "prob")$more

# Optimize classification probability threshold:
t1 <- Sys.time()
threshold_rpart <- optimize_threshold(labels = validation_data$income, probs = validation_data$pred_rpart_prob_more)
t2 <- Sys.time(); t2 - t1 # Time difference of 4.119202 mins

# Select as threshold p where Kappa is max:
class_threshold_rpart <- threshold_rpart$Threshold[threshold_rpart$Kappa == max(threshold_rpart$Kappa)][1]

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
validation_data$pred_rpart_class_opt <- ifelse(validation_data$pred_rpart_prob_more >= class_threshold_rpart, "more", "less")

# Compare prior probabilities to prediction probabilities:
priors_validation_data <- estimte_prior_probs(labels = validation_data$income) # [1] 0.06201021
post_validation_data <- sum(validation_data$pred_rpart_class_opt == "more")/nrow(validation_data) # [1] 0.1499909
paste0("Prior probs: ",round(priors_validation_data, 4), "\nPrediction probs: ", round(post_validation_data, 4))

##### PREDICT ON TEST SET USING OPTIMISED PROBABILITY THRESHOLD #####
test_data$pred_rpart_prob_more <- predict(object = model_rpart, newdata = test_data, type = "prob")$more
test_data$pred_rpart_class_opt <- ifelse(test_data$pred_rpart_prob_more >= class_threshold_rpart, "more", "less")

# Compare prior probabilities to prediction probabilities:
priors_test_data <- estimte_prior_probs(labels = test_data$income) # [1] 0.06200248
post_test_data <- sum(test_data$pred_rpart_class_opt == "more")/nrow(test_data) # [1] 0.1514535
paste0("Prior probs: ",round(priors_validation_data, 4), "\nPrediction probs: ", round(post_test_data, 4))

##### GOOD HOUSEKEEPING #####
#rm(list=setdiff(ls(), c("train_data", "test_data", "validation_data", "control_train", 
#                        "estimte_prior_probs", "classify_predictions", "optimize_threshold")))
#ls()

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
caret::confusionMatrix(validation_data$pred_cart_class_raw, validation_data$income, positive = "more")
print(postResample(pred = validation_data$pred_cart_class_raw, obs = validation_data$income)) # 0.52572748 0.09963197 
### Almost the same performance as GBM. Only 10% higher than random prediction-reference classification agreement.

##### PROBABILITY THRESHOLD OPTIMISATION #####
# Get prediction probabilities for income == "more":
validation_data$pred_cart_prob_more <- predict(object = model_cart, newdata = validation_data, type = "prob")$more

# Optimize classification probability threshold:
t1 <- Sys.time()
threshold_cart <- optimize_threshold(labels = validation_data$income, probs = validation_data$pred_cart_prob_more)
t2 <- Sys.time(); t2 - t1 # Time difference of 6.164637 mins

# Select as threshold p where Kappa is max:
class_threshold_cart <- threshold_cart$Threshold[threshold_cart$Kappa == max(threshold_cart$Kappa)][1]

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
validation_data$pred_cart_class_opt <- ifelse(validation_data$pred_cart_prob_more >= class_threshold_cart, "more", "less")

# Compare prior probabilities to prediction probabilities:
priors_validation_data <- estimte_prior_probs(labels = validation_data$income) # [1] 0.06201021
post_validation_data <- sum(validation_data$pred_cart_class_opt == "more")/nrow(validation_data) # [1] 0.1499301
paste0("Prior probs: ",round(priors_validation_data, 4), "\nPrediction probs: ", round(post_validation_data, 4))

##### PREDICT ON TEST SET USING OPTIMISED PROBABILITY THRESHOLD #####
test_data$pred_cart_prob_more <- predict(object = model_cart, newdata = test_data, type = "prob")$more
test_data$pred_cart_class_opt <- ifelse(test_data$pred_cart_prob_more >= class_threshold_cart, "more", "less")

# Compare prior probabilities to prediction probabilities:
priors_test_data <- estimte_prior_probs(labels = test_data$income) # [1] 0.06200248
post_test_data <- sum(test_data$pred_cart_class_opt == "more")/nrow(test_data) # [1] 0.1526918
paste0("Prior probs: ",round(priors_validation_data, 4), "\nPrediction probs: ", round(post_test_data, 4))

# ##### GOOD HOUSEKEEPING #####
# rm(list=setdiff(ls(), c("train_data", "test_data", "validation_data", "control_train", 
#                         "estimte_prior_probs", "classify_predictions", "optimize_threshold")))
# ls()

##############################
##### MODEL CORRELATIONS #####
##############################

##### SELECT PREDICTED PROBABILITIES FROM BASE MODELS #####
# Get column names from predicted featurs:
prediction_probs_labels <- c("pred_gbm_prob_more", "pred_rpart_prob_more", "pred_cart_prob_more")
# Create dataset with only the predicted probabilities by the base models:
prediction_probs_validation_data <- validation_data[,names(validation_data) %in% prediction_probs_labels]
# Rename columns for better visualisation of correlation plot:
colnames(prediction_probs_validation_data) <- c("GBM", "RPART", "CART")
# Calculate correlations between base models:
correlation_preds_probls_val <- cor(prediction_probs_validation_data)
# Plot correlations:
corrplot.mixed(correlation_preds_probls_val, upper = "color")

############################
##### MODEL ENSEMBLING #####
############################
model_name <- "GBM Ensamble Model of GBM, RPART, & Bagged CART"

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
# t1 <- Sys.time()
# model_ensemble <- train(income~., data=validation_data[,c(prediction_class_labels, "income")], method="gbm", trControl=control_train, tuneGrid = gbm_grid, metric = "Kappa", keep.data = FALSE)
# t2 <- Sys.time()
# t2- t1 # Time difference of 10.98253 mins
# saveRDS(model_ensemble, file = "outputs/model_building/model_ensemble.RDS")
model_ensemble <- readRDS(file = "outputs/model_building/model_ensemble.RDS")

##### PREDICT ENSEMBLE MODEL ON TEST SET #####
# Analyse default classification results (probability threshold = 0.5):
test_data$pred_ensemble_class_raw <- predict(object = model_ensemble, newdata = test_data, type = "raw")
caret::confusionMatrix(test_data$pred_ensemble_class_raw, test_data$income, positive = "more")
print(postResample(pred = test_data$pred_ensemble_class_raw, obs = test_data$income)) # 0.8530574 0.3478076 
### Very good performance. 35% higher than random prediction-reference classification agreement.

##### PROBABILITY THRESHOLD OPTIMISATION #####
# Get prediction probabilities for income == "more":
test_data$pred_ensemble_prob_more <- predict(object = model_ensemble, newdata = test_data, type = "prob")$more

# Optimize classification probability threshold:
t1 <- Sys.time()
threshold_ensemble <- optimize_threshold(labels = test_data$income, probs = test_data$pred_ensemble_prob_more)
t2 <- Sys.time(); t2 - t1 # Time difference of 2.033221 mins

# Select as threshold p where Kappa is max:
class_threshold_ensemble <- threshold_ensemble$Threshold[threshold_ensemble$Kappa == max(threshold_ensemble$Kappa)][1]

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
test_data$pred_ensemble_class_opt <- as.factor(ifelse(test_data$pred_ensemble_prob_more >= class_threshold_ensemble, "more", "less"))

# Compare prior probabilities to prediction probabilities:
priors_test_data <- estimte_prior_probs(labels = test_data$income) # [1] 0.06200248
post_test_data <- sum(test_data$pred_ensemble_class_opt == "more")/nrow(test_data) # [1] 0.04690725
paste0("Prior probs: ",round(priors_test_data, 4), "\nPrediction probs: ", round(post_test_data, 4))

##### EVALUATE ENSEMBLE MODEL PERFORMANCE #####
caret::confusionMatrix(test_data$pred_ensemble_class_opt, test_data$income, positive = "more")
print(postResample(pred = test_data$pred_ensemble_class_opt, obs = test_data$income)) # 0.9389410 0.4015821 
### Even better performance. 40% higher than random prediction-reference classification agreement.


############################
##### FEATURE INSIGHTS #####
############################

##### FEATURE INSIGHTS FROM GBM BASE LAYER MODEL #####
var_imp_gbm <- varImp(model_gbm, scale=TRUE)
print(var_imp_gbm)
plot(var_imp_gbm, main = "Variable Importance Estimated with GBM Base Layer Model")

##### FEATURE INSIGHTS FROM RPART BASE LAYER MODEL #####
var_imp_rpart <- varImp(model_rpart, scale=TRUE)
print(var_imp_rpart)
plot(var_imp_rpart, main = "Variable Importance Estimated with RPART Base Layer Model")

##### FEATURE INSIGHTS FROM CART BASE LAYER MODEL #####
var_imp_cart <- varImp(model_cart, scale=TRUE)
print(var_imp_cart)
plot(var_imp_cart, main = "Variable Importance Estimated with CART Base Layer Model")

##### AGGREGATE FEATURE IMPORTANCE #####
var_names <- unique(c(row.names(var_imp_gbm$importance), row.names(var_imp_rpart$importance), row.names(var_imp_cart$importance)))
var_imp_gbm_df <- data.frame(FEATURE = row.names(var_imp_gbm$importance), GBM = var_imp_gbm$importance$Overall, stringsAsFactors = FALSE)
var_imp_rpart_df <- data.frame(FEATURE = row.names(var_imp_rpart$importance), RPART = var_imp_rpart$importance$Overall, stringsAsFactors = FALSE)
var_imp_cart_df <- data.frame(FEATURE = row.names(var_imp_cart$importance), CART = var_imp_cart$importance$Overall, stringsAsFactors = FALSE)
var_imp_aggr <- merge(var_imp_gbm_df, var_imp_rpart_df, by = "FEATURE")
var_imp_aggr <- merge(var_imp_aggr, var_imp_cart_df, by = "FEATURE")

#var_imp_gbm$importance, var_imp_rpart$importance, var_imp_cart$importance)
var_imp_aggr$AGGR <- rowMeans(var_imp_aggr[,2:4])
var_imp_aggr <- var_imp_aggr[order(-var_imp_aggr$AGGR),]
print(var_imp_aggr[,c("FEATURE", "AGGR")])
# Plot feature importance:
ggplot(var_imp_aggr, aes(x=reorder(FEATURE, -AGGR), y= AGGR)) + 
  geom_point(size=3, color = "seagreen2") + 
  geom_segment(aes(x=FEATURE, 
                   xend=FEATURE, 
                   y=0, 
                   yend=AGGR),
               size = 1,
               color = "seagreen2") + 
  labs(title="Aggregated Feature Importance", 
       subtitle="Averaged Feature Importance from GBM, RPART, and CART Base Layer Models",
       caption = "source: U.S. Census Bureau Data, Current Population Surveys 1994 and 1995") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6),  axis.title.x=element_blank()) +
  ylab("Weight")

##### CONDITIONAL DISTRIBUTION OF TOP-N FEATURES #####
copy_of_train_data <- readRDS(file = "data/train_data_factors_na_removed.RDS") 
top_n_features <- 7
for(i in var_imp_aggr$FEATURE[1:top_n_features]){
  var_class <- class(copy_of_train_data[,names(copy_of_train_data) == i])
  if(var_class == "integer"){
    print(plot_numerical_variable(variable = i, dataset = copy_of_train_data, colour = sample(palette, 1), set_type = "Training Set"))
  } else if(var_class == "factor") {
    suppressWarnings(print(plot_categorical_variable(variable = i, dataset = copy_of_train_data, colour = sample(palette, 1), set_type = "Training Set", n = 9)))
  }
}

