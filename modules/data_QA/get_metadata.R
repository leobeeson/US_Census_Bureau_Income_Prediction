library(stringr)
library(dplyr)

##### READ IN METADATA FILE #####
metadata_file <- readLines("data/census_income_metadata.txt")

##### GET VARIABLES' DESCRIPTIONS AND ID #####
clean_variables_description <- function(line){
  out <- str_sub(string = line, start = 2)
  out <- trimws(out)
  out <- str_split(string = out, pattern = "\t+")
  return(out)
}
var_names <- metadata_file[24:68]
var_names_list <- lapply(var_names, clean_variables_description)
var_names_df <- data.frame(matrix(unlist(var_names_list), nrow = 45, byrow = TRUE), stringsAsFactors = FALSE)
names(var_names_df) <- c("var_description", "var_ID")
rm(var_names, var_names_list)

##### GET VARIABLES' TYPE AND NUMBER OF DISTINCT VALUES #####
clean_variables_types <- function(line){
  out <- str_sub(string = line, start = 2)
  out <- str_split(string = out, pattern = " (?=distinct)|\\(|\\)", simplify = TRUE)
  out <- out[c(1,length(out)-1,length(out))]
  out <- trimws(out)
  return(out)
}
var_types <- metadata_file[82:121]
var_types_list <- lapply(var_types, clean_variables_types)
var_types_df <- data.frame(matrix(unlist(var_types_list), nrow = 40, byrow = TRUE), stringsAsFactors = FALSE)
names(var_types_df) <- c("num_distinct_vals", "var_description", "var_type")
rm(var_types, var_types_list)

##### GET VARIABLES' DISTINCT VALUES #####
clean_unique_values <- function(line){
  out <- str_sub(string = line, start = 1, end = nchar(line) - 1)
  out <- gsub(pattern = "\\|", replacement = "", x = out)
  out <- str_split(string = out, pattern = ":", n = 2, simplify = TRUE)
  out <- trimws(out)
  return(out)
}

var_unique_vals <- metadata_file[143:184]
var_unique_vals_list <- lapply(var_unique_vals, clean_unique_values)
var_unique_vals_df <- data.frame(matrix(unlist(var_unique_vals_list), nrow = 42, byrow = TRUE), stringsAsFactors = FALSE)
names(var_unique_vals_df) <- c("var_description", "var_factors")
rm(var_unique_vals, var_unique_vals_list, metadata_file)

##### NORMALISE AND CLEAN VARIABLE NAMES #####
var_names_df$var_description[var_names_df$var_description == "industry code"] <- "detailed industry recode"
var_names_df$var_description[var_names_df$var_description == "occupation code"] <- "detailed occupation recode"
var_names_df$var_description[var_names_df$var_description == "enrolled in edu inst last wk"] <- "enroll in edu inst last wk"
var_names_df$var_description[var_names_df$var_description == "marital status"] <- "marital stat"
var_names_df$var_description[var_names_df$var_description == "mace"] <- "race"
var_names_df$var_description[var_names_df$var_description == "hispanic Origin"] <- "hispanic origin"
var_names_df$var_description[var_names_df$var_description == "divdends from stocks"] <- "dividends from stocks"
var_names_df$var_description[var_names_df$var_description == "tax filer status"] <- "tax filer stat"
var_names_df <- rbind(var_names_df, c("year", "YEAR"))

excluded <- c("adjusted gross income", "federal income tax liability", "total person earnings", "total person income", "taxable income amount")
var_names_df <- var_names_df[!var_names_df$var_description %in% excluded, ]
rm(excluded)

##### JOIN VARIABLE METADATA #####
metadata_df <- left_join(var_names_df, var_types_df, by = "var_description") %>%
  left_join(., var_unique_vals_df, by = "var_description")

metadata_df <- metadata_df[-25, ]
metadata_df$var_description[metadata_df$var_description == "fill inc questionnaire for veteran's admin"] <- "fill inc questionnaire for veterans admin"
metadata_df$var_description <- str_replace_all(string = metadata_df$var_description, pattern = " ", replacement = "_")
rm(var_names_df, var_types_df, var_unique_vals_df)

saveRDS(object = metadata_df, file = "resources/metadata_df.RDS")
