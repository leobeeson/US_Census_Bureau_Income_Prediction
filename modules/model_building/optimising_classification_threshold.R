library(caret)

# Function for calculating the prior probabilitiy of income == "more":
estimte_prior_probs <- function(labels, positive_label = "more"){
  prior_probs <- length(labels[labels == positive_label])/length(labels)
  return(prior_probs)
}

# Function for classifying prediction probabilities depending on a threshold parameter:
classify_predictions <- function(probs_possitive_class, threshold, positive_label, negative_lable){
  class_preds <- as.factor(ifelse(probs_possitive_class < threshold, negative_lable, positive_label))
  return(class_preds)
}

#Function for optimisation of classification probability threshold:
optimize_threshold <- function(labels, probs_possitive_class, step_size = 0.0001, positive_label = "more", negative_lable = "less"){
  steps <- seq(from = 0 + step_size, to = 1 - step_size, by = step_size)
  kappas <- data.frame(Threshold = steps, Kappa = rep(0, length(steps)))
  kappas$Kappa <- sapply(kappas$Threshold, function(x) unname(postResample(classify_predictions(probs_possitive_class, x , positive_label, negative_lable), labels)[2]))
  return(kappas)
}
