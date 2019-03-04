library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
source("modules/EDA/plot_categorical_variables.R")
source("modules/EDA/plot_numerical_variables.R")

##### READ IN TRAIN AND TEST DATASETS #####
train_data <- readRDS(file = "data/train_data.RDS")
#train_data$dataset <- "train"
test_data <- readRDS(file = "data/test_data.RDS")
#test_data$dataset <- "test"
#full_data <- rbind(train_data, test_data)
#full_data$dataset <- as.factor(full_data$dataset)
#rm(train_data, test_data)

##### IDENTIFY CATEGORICAL FROM NUMERICAL VARIABLES #####
vars_cat <- names(train_data)[unname(sapply(train_data, class)) == "factor"]
vars_cat <- vars_cat[vars_cat != "income"]
vars_num <- names(train_data)[unname(sapply(train_data, class)) == "integer"]

#################################
##### CATEGORICAL VARIABLES #####
#################################

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
for(v in vars_cat){
  for(t in c("Train set", "Test set")){
    png(paste0("outputs/EDA/Plots_Categorical_Vars/",v,"_",tolower(str_split(t, " ")[[1]][1]),"_set.png"))
    dataset <- if(t == "Train set"){train_data} else {test_data}
    p <- plot_categorical_variable(variable = v, dataset = dataset, colour = sample(palette, 1), n = n, set_type = t)
    print(p)
    dev.off()
  }
}

#################################
##### NUMERICAL VARIABLES #####
#################################

##### NUMERICAL VARIABLES: UNIVARIATE DISTRIBUTIONS AND CONDITIONAL DISTRIBUTIONS VS. INCOME #####
for(v in vars_num){
  for(t in c("Train set", "Test set")){
    ppi <- 300
    png(paste0("outputs/EDA/Plots_Numerical_Vars/",v,"_",tolower(str_split(t, " ")[[1]][1]),"_set.png"), width=8*ppi, height=6*ppi, res=ppi)
    dataset <- if(t == "Train set"){train_data} else {test_data}
    p <- plot_numerical_variable(variable = v, dataset = dataset, colour = sample(palette, 1), set_type = t)
    print(p)
    dev.off()
  }
}

##### HOUSECLEANING #####
rm(train_data, test_data, dataset, outcome_names, p, n, palette, ppi, t, v, vars_cat, vars_num)
