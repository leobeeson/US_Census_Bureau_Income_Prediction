library(stringr)
library(dplyr)

##### READ IN TEST SET #####
test_data <- read.csv(file = "data/census_income_test.csv")

##### READ IN CURATED METADATA #####
metadata_df <- readRDS(file = "resources/metadata_df.RDS")

##### ASSIGN VARIABLE NAMES TO TEST SET #####
names(test_data) <- c(metadata_df$var_description, "income")

##### SANITY CHECKS #####
check_sanity_distinct <- function(var_name){
  vals <- trimws(as.character(test_data[,names(test_data) == var_name]))
  unique_vals <- sort(unique(vals))
  num_unique_vals <- length(unique_vals)
  unique_vals <- unique_vals[unique_vals != "?"]
  unique_vals <- paste(unique_vals, collapse = ", ")
  out <- list(var_name = var_name, sc_unique_vals = unique_vals, sc_num_unique_vals = num_unique_vals)
  return(out)
}

##### GET UNIQUE VALUES FROM TEST FILE  #####
sc_uniques <- lapply(names(test_data)[!names(test_data) %in% c("income")], check_sanity_distinct)
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

saveRDS(object = metadata_df, file = "resources/metadata_df_sanity_check_test_set.RDS")

##### AUDIT NON-MATCHING UNIQUE VALUES #####
set_diff_vector_elements <- function(string_1, string_2){
  vec_1 <- trimws(unlist(str_split(string = string_1, pattern = ",")))
  vec_2 <- trimws(unlist(str_split(string = string_2, pattern = ",")))
  diffs <- setdiff(vec_1, vec_2)
  return(diffs)
}

# Create subset of observations from categorical variables with non-matching unique values:
audit_test_df <- metadata_df %>%
  filter(var_type != "continuous") %>%
  filter(sc_unique_vals_bool == FALSE)

# Identify which unique values for the relevant variable appears in the training set but not in the test set:
factors_not_in_test_set <- set_diff_vector_elements(audit_test_df$var_factors[1], audit_test_df$sc_unique_vals[1])
cat(factors_not_in_test_set)
# Missing factor in test set: Grandchild <18 ever marr not in subfamily

# Double check missing factors:
grep(pattern = factors_not_in_test_set, x = metadata_df$var_factors)        # [1] 23
grep(pattern = factors_not_in_test_set, x = metadata_df$sc_unique_vals)     # integer(0)
