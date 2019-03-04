library(stringr)
library(dplyr)

##### READ IN TRAIN SET #####
train_data <- read.csv(file = "data/census_income_learn.csv")

##### READ IN CURATED METADATA #####
metadata_df <- readRDS(file = "resources/metadata_df.RDS")

##### ASSIGN VARIABLE NAMES TO TRAIN SET #####
names(train_data) <- c(metadata_df$var_description, "income")

##### SANITY CHECKS #####
check_sanity_distinct <- function(var_name){
  vals <- trimws(as.character(train_data[,names(train_data) == var_name]))
  unique_vals <- sort(unique(vals))
  num_unique_vals <- length(unique_vals)
  unique_vals <- unique_vals[unique_vals != "?"]
  unique_vals <- paste(unique_vals, collapse = ", ")
  out <- list(var_name = var_name, sc_unique_vals = unique_vals, sc_num_unique_vals = num_unique_vals)
  return(out)
}

##### GET UNIQUE VALUES FROM LEARNING FILE (TRAINING DATA) #####
sc_uniques <- lapply(names(train_data)[!names(train_data) %in% c("income")], check_sanity_distinct)
sc_var_description <- unlist(lapply(sc_uniques, function(x) x[[1]]))
sc_unique_vals <- unlist(lapply(sc_uniques, function(x) x[[2]]))
sc_num_unique_vals <- unlist(lapply(sc_uniques, function(x) x[[3]]))    
rm(sc_uniques)

##### CHECK VAR DESCRIPTIONS #####
metadata_df$sc_var_description <- sc_var_description
metadata_df$sc_var_description_bool <- metadata_df$var_description == metadata_df$sc_var_description
rm(sc_var_description)

##### CHECK NUMBER OF UNIQUE VALUES #####
metadata_df$sc_num_unique_vals <- sc_num_unique_vals
metadata_df$sc_num_unique_vals_bool <- metadata_df$num_distinct_vals == metadata_df$sc_num_unique_vals
rm(sc_num_unique_vals)

##### CHECK UNIQUE VALUES #####
compare_vector_elements <- function(string_1, string_2){
  vec_1 <- trimws(unlist(str_split(string = string_1, pattern = ",")))
  vec_2 <- trimws(unlist(str_split(string = string_2, pattern = ",")))
  equality_check <- identical(sort(vec_1), sort(vec_2))
  return(equality_check)
}
metadata_df$sc_unique_vals <- sc_unique_vals
metadata_df$sc_unique_vals[metadata_df$var_factors == "continuous"] <- "continuous"
metadata_df$sc_unique_vals_bool <- mapply(compare_vector_elements, metadata_df$var_factors, metadata_df$sc_unique_vals)
rm(sc_unique_vals)

saveRDS(object = metadata_df, file = "resources/metadata_df_sanity_check_training_set.RDS")
