# prepare data


  # Discretize function
  discretize <- function(data_frame, target_var, predictors = 'all') {
    # Checks
    if (!is.data.frame(data_frame)) {
      stop("Input 'data_frame' must be a data.frame.")
    }
    if (!target_var %in% colnames(data_frame)) {
      stop("The input target variable '", target_var, "' does not exist in the dataframe.")
    }
    if (!(is.factor(data_frame[[target_var]]) || is.character(data_frame[[target_var]]))) {
      stop("The target variable must be either a factor or character.")
    }

    # Select the target variable
    target <- data_frame[, target_var, drop = FALSE]

    if (identical(predictors, 'all')) {
      predictor_df <- data_frame[, colnames(data_frame) != target_var, drop = FALSE]
    }
    else {
      predictor_df <- data_frame[, predictors, drop = FALSE]
    }

    # Apply is.numeric to each column
    numeric_cols <- sapply(predictor_df, is.numeric)

    # Isolate the numeric data
    numeric_data <- predictor_df[, numeric_cols, drop = FALSE]

    # Take non-numeric data (excluding the target variable)
    non_numeric_cols <- !numeric_cols
    non_numeric_data <- predictor_df[, non_numeric_cols, drop = FALSE]
    non_numeric_data <- non_numeric_data[, colnames(non_numeric_data) != target_var, drop = FALSE]

    # Prepare numeric variables with the target variable at the end for the mdlp function
    new_data <- cbind(numeric_data, target)

    # Apply the mdlp function and extract the results
    disc <- discretization::mdlp(new_data)
    disc <- disc$Disc.data

    # Add non-numeric variables back
    full_data <- cbind(non_numeric_data, disc)

    # Return the final discretized data
    return(full_data)
  }

  # Train-test split
  train_test_split <- function(data_frame, target_var, predictors = 'all', train_size) {
    index <- caret::createDataPartition(data_frame[[target_var]], p = train_size, list = FALSE, times = 1)

    if (identical(predictors, 'all')) {
      train_data <- data_frame[index, ]
      test_data <- data_frame[-index, ]
    }
    else {
      train_data <- data_frame[index, predictors]
      test_data <- data_frame[-index, predictors]
    }

    y_train <- train_data[[target_var]]
    y_test <- test_data[[target_var]]

    X_train <- train_data[, setdiff(names(train_data), target_var), drop = FALSE]
    X_test <- test_data[, setdiff(names(test_data), target_var), drop = FALSE]

    return(list(X_train = X_train, y_train = y_train, X_test = X_test, y_test = y_test))
  }






