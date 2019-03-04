library(dplyr)
library(ggplot2)
library(stringr)

##### REPLACE OUTCOME LABELS FOR FACET ANALYSIS #####
outcome_names <- list("0"="Income < USD 50k", "1"="Income > USD 50k", '(all)'="Total Respondent")
outcome_labeller <- function(variable,value){
  return(outcome_names[value])
}

##### FUNCTION FOR PROPER CASING TITLES #####
proper_case <- function(x){
  out <- str_replace_all(string = x, pattern = "_", " ")
  out <- paste0(toupper(str_sub(out, 1, 1)), tolower(str_sub(out, 2)))
} 

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
         caption = "U.S. Census Bureau Data, Current Population Surveys 1994 and 1995")
  return(p)
}
