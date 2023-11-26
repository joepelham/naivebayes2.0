library(R6)

# Define the naivebayes class
naivebayes <- R6::R6Class("naivebayes",
                          public = list(
                            # Constructor for training the model
                            fit = function(X_train, y_train, smoothing_param = 1) {
                              # Validation checks for y_train input
                              if (!is.vector(y_train)) {
                                stop("y_train must be a vector.")
                              }
                              if (!(is.factor(y_train) || is.character(y_train))) {
                                stop("y_train must be a factor or character vector.")
                              }
                              # Validation checks for X_train input
                              if (!is.data.frame(X_train)) {
                                stop("X_train must be a dataframe.")
                              }

                              if (!all(sapply(data, function(col) !(is.factor(col) || is.character(col))))) {
                                stop("Columns in X_train must be either factors or characters.")
                              }

                              # Calculate prior probabilities
                              prior_probs <- table(y_train) / length(y_train)

                              # Create an empty list to store conditional probability values
                              conditional_probs_list <- list()

                              # Calculate conditional probabilities
                              for (i in colnames(X_train)) {
                                contingency_table <- table(X_train[[i]], y_train) + smoothing_param
                                conditional_probs <- prop.table(contingency_table, 2)
                                conditional_probs_list[[i]] <- conditional_probs
                              }

                              # Extract the number of unique classes of the target variable
                              classes <- unique(y_train)

                              # store prior probs, conditional probs, and # of classes in the R6 object
                              private$model <- list(
                                prior_probabilities = as.table(prior_probs),
                                conditional_probabilities = conditional_probs_list,
                                unique_classes = classes
                              )
                            },

                            # Create a print method that prints all of the information calculated in the fit()
                            print = function(model) {
                              print(private$model)
                            },

                            # Create a predict method
                            predict = function(X_test) {
                              # call the relevant information from inside the model
                              prior_probs <- private$model$prior_probabilities
                              cond_probs <- private$model$conditional_probabilities
                              classes <- private$model$unique_classes

                              # Create a new vector for predictions, the same length as X_test
                              predictions <- character(nrow(X_test))
                              # Create a new matrix, with the same number of rows as the X_test and the same number of columns as
                              # the number of classes. This matrix will store prediction probabilities.
                              probabilities <- matrix(0, nrow = nrow(X_test), ncol = length(classes), dimnames = list(NULL, classes))

                              # Calculate predictions
                              # Iterate through each row of X_test
                              for (i in seq_len(nrow(X_test))) {
                                values <- as.character(X_test[i, ])
                                likelihoods <- numeric(length(classes))
                                #Interate through each class
                                for (j in seq_along(classes)) {
                                  likelihood <- prior_probs[j]
                                  # Iterate through each value of the selected row
                                  for (k in seq_along(values)) {
                                    variable <- names(X_test)[k]
                                    level <- values[k]

                                    likelihood <- likelihood * cond_probs[[variable]][level, j]
                                  }
                                  # Store likelihoods
                                  likelihoods[j] <- likelihood
                                }
                                # Normalize probabilities and finalize predictions
                                probabilities[i, ] <- likelihoods / sum(likelihoods)
                                predicted_class_index <- which.max(likelihoods)
                                predictions[i] <- classes[predicted_class_index]
                              }

                              # Store predictions and probabilities in the R6 object
                              private$predictions <- predictions
                              private$probabilities <- probabilities

                            },

                            # method to extract the predictions from the R6 class
                            get_predictions = function() {
                              if (is.null(private$predictions)) {
                                warning("There are currently no stored predictions. Please run the 'predict' method first.")
                                return(NULL)
                              } else {
                                return(private$predictions)
                              }
                            },

                            # Method to extract stored probabilities from the R6 class
                            get_prediction_probas = function() {
                              if (is.null(private$probabilities)) {
                                warning("There are currently no stored prediction probabilities. Please run the 'predict' method first.")
                                return(NULL)
                              } else {
                                return(private$probabilities)
                              }
                            },

                            # Method for to evaluate model and calculate metrics
                            test = function(y_test, f1_beta = 1) {
                              # Extract relevant information from the model and set y_test input to "actual"
                              actual = y_test
                              predicted = private$predictions

                              # Form confusion matrix
                              confusion_mat <- table(actual, predicted)
                              # Calculate accuracy (function included below)
                              acc <- calc_accuracy(actual, predicted)
                              # Calculate precision (function included below)
                              precisions <- calc_precision(actual, predicted)
                              # Calculate recall (function included below)
                              recalls <- calc_recall(actual, predicted)
                              # Calculate f1 score (function included below)
                              f1 <- calc_f1(precisions[1], recalls[1], beta = f1_beta)

                              # Store the confusion matrix and metrics
                              private$stored_metrics <- list(
                                confusion_matrix = confusion_mat,
                                accuracy = acc,
                                precision = precisions,
                                recall = recalls,
                                f1_score = as.numeric(f1)
                              )
                            },

                            # Create new method "summary()" to summarize and print the stored metrics
                            summary = function() {
                              if (is.null(private$stored_metrics)) {
                                print("No metrics available. Please run the 'test' method before calling the summary method.")
                              } else {
                                cat("Confusion Matrix:\n")
                                print(private$stored_metrics$confusion_matrix)

                                cat("\nAccuracy:", private$stored_metrics$accuracy, "\n")

                                cat("\nPrecision Scores:\n")
                                print(private$stored_metrics$precision)

                                cat("\nRecall Scores:\n")
                                print(private$stored_metrics$recall)

                                cat("\nF1 Score:",private$stored_metrics$f1_score, "\n")
                              }
                            }

                          ),

                          # Private variables
                          private = list(
                            model = NULL,
                            stored_metrics = NULL,
                            predictions = NULL,
                            probabilities = NULL
                          )
)
