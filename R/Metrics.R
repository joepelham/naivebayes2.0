
# Confustion Matrix function
confusion_matrix = function(actual, predicted) {
  confusion_matrix <- table(actual, predicted)
  return(confusion_matrix)
}

# Accuracy function
calc_accuracy = function(actual, predicted) {
  confusion_mat <- table(actual, predicted)
  acc <- sum(diag(confusion_mat)) / sum(confusion_mat)

  return(acc)
}

# Recall function
calc_recall = function(actual, predicted) {
  confusion_matrix <- table(actual, predicted)
  recalls <- diag(confusion_matrix) / rowSums(confusion_matrix)
  macro_mean_prec <- mean(recalls, na.rm = TRUE)

  recalls_list <- c("Macro Average Recall Score" = macro_mean_prec,
                    "Class Recall Scores" = recalls)

  return(recalls_list)
}

# Precision function
calc_precision = function(actual, predicted) {
  confusion_matrix <- table(actual, predicted)
  precisions <- diag(confusion_matrix) / colSums(confusion_matrix)
  macro_mean_rec <- mean(precisions, na.rm = TRUE)

  precision_list <- c("Macro Average Precision Score" = macro_mean_rec,
                      "Class Precision Scores" = precisions)

  return(precision_list)
}

# f1 function
calc_f1 = function(precision, recall, beta = 1) {
  f1 <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

  return(f1)
}
