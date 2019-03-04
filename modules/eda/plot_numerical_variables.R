if(!("plyr" %in% (.packages()))){library(plyr)}
if(!("dplyr" %in% (.packages()))){library(dplyr)}
if(!("ggplot2" %in% (.packages()))){library(ggplot2)}
if(!("stringr" %in% (.packages()))){library(stringr)}
library(gridExtra)

##### FUNCTION FOR PROPER CASING TITLES #####
proper_case <- function(x){
  out <- str_replace_all(string = x, pattern = "_", " ")
  out <- paste0(toupper(str_sub(out, 1, 1)), tolower(str_sub(out, 2)))
} 

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
               caption = "U.S. Census Bureau Data, Current Population Surveys 1994 and 1995") +
          geom_text(data=var_median_summary, aes(x=income, y=var_median, label=paste0("Median: ",var_median), colour=income), size = 3, vjust = -0.5, show.legend = FALSE)
    
  p <- grid.arrange(p_1, p_2, ncol=2)
  
  return(p)
}
