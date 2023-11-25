\name{naivebayes}
\alias{naivebayes}
\alias{fit}
\alias{print}
\alias{predict}
\alias{get_predictions}
\alias{get_prediction_probas}
\alias{test}
\alias{summary}

\title{
Naive Bayes Calculation R6 Class
}
\description{
\code{naivebayes} is an R6 Class Generator that creates an R6 class capable of fitting a Naive Bayes model, using that model to predict values, and evalutating those predictions, all of which take place employing functionalities within the R6 class object.
}
\usage{
# Class 'R6ClassGenerator' <naivebayes> object generator
naivebayes_object <- naivebayes$new()

# Methods:
# fit the model
naivebayes_object$fit(X_train,y_train, smoothing_param = 1)
# print information relative to the fit model
naivebayes_object$print()
# Predict values of \code{y} in test data
naivebayes_object$predict(X_test)
# Extract vector predictions from the object
naivebayes_object$get_predictions()
# Extract matrix of prediction probabilities from the object
naivebayes_object$get_prediction_probas()
# Perform metrics on results of {\code$predict}, comparing with \code{y_test}
naivebayes_object$test(y_test)
# Examine the different metrics
nnaivebayes_object$summary()
}
\arguments{
\item{X_train}{
A datafrome of training data excluding the target variable \code{y}. All variables must be either categorical or factors.
}
\item{X_test}{
A datafrome of test data excluding the target variable \code{y}. All variables must be either categorical or factors.
}
\item{y_train}{
A categorical or factor vector \code{y} for the training data.
}
\item{y_test}{
A categorical or factor vector \code{y} for the test data.
}
\item{smoothing_param}{
With a default value of \code{1} (Known as Laplace smoothing), the smoothing parameter is used in the \code{fit} method. The smoothing paramter addresses the issue of 0 probabilities during the calculation of conditional likelihoods.
}
}
\details{
\code{$fit}{
A method that uses input training data \code{X_train} & \code{y_train} as well as a smoothing parameter \code{smoothing_param} to compute a multinomial Naive Bayes model. The fit calculates the prior probabilities (the probability of each class of class of \code{y}) appearing in the training data), the conditional likelihoods (the likelihood of a level of a predictor variable given a level of \code{y}, and the number of unique classes in target variable \code{y}.)
}

\code{$print}{
Prints the results calculated during the fit (the prior probabilities, the conditional likelihoods, and the unique classes of \code{y}).
}

\code{$predict}{
A method that predict values of the target variable \code{y} using the model created during the \code{fit} method as well as test data dataframe \code{X_test}. In order to do so, the method multiplies (for each row of \code{X_test}) the prior probability for a given class by each appropriate conditional likelihood (depending on the values of the given row). The method calculates the likelihoods and selects the class with the highest likelhood, given the values of each row of \code{X_test}.
}

\code{$get_predictions}{
Extract a vector of predictions from the object. (must be run after \code{$predict})
}

\code{$get_prediction_probas}{
Extract a matrix of prediction probabilities from the object. (must be run after \code{$predict})
}

\code{$test}{
A method that performs metrics on the results of \code{$predict}, comparing with the actual values \code{y_test}. A confusion matrix is calculated, as well as the accuracy score, a list of precision scores (a macro averaged precision score and a precision score for each classof \code{y}), a list of recall scores, (a macro averaged recall score and a recall score for each class for each class of \code{y}).
}

\code{$summary}{
A method to examine different metrics (calculated by \code{$test} method)
}
}


\examples{
# Import data
heart <-naivebayes2.0::heart
# Process data (functions also present in the naivebayes2.0 package)
discdta <- discretize(heart, "HeartDisease", predictors = c('Age', 'Sex'))
ML_list <- train_test_split(discdta, "HeartDisease",train_size = .8)

# Generate new R6 class and perform fit, predict, and test (with print and summary methods as well)
nb <- naivebayes$new()
nb$fit(ML_list$X_train,ML_list$y_train)
nb$print()
nb$predict(ML_list$X_test)
nb$test(ML_list$y_test)
nb$summary()
}

