\name{train_test_split}
\alias{train_test_split}

\title{
Train/Test Split
}
\description{
Using the function \code{createDataPartition} from the \code{caret} package, this function takes a dataframe, the name of the target variable (y), the names of predictor varaibles, and the desired size for the training set and returns a list of four objects (\code{X_train, y_train, X_test, and y_test}) ready for the use in the calculation of Naive Bayes or other machine learning algorithms.
}
\usage{
train_test_split(data_frame, target_var, predictors = 'all', train_size)
}
\arguments{
\item{data_frame}{
A datafrome with mixed or numeric data, including all of the variables, both predictor and target variables.
}
  \item{target_var}{
The name of the column that will be the predicted variable during the modelisation. This variable is also necessary to perform discretization. Note, this column must be a factor or character variable.
}
  \item{predictors}{
A list of column names for predictor variables intended for use in the modelization. When set to \code{predictors = 'all'}, the default setting, all variables other than the input target variable will be entered. Non-numeric columns will automatically be excluded from the discretization process but reattached in the dataframe returned by the function.
}
  \item{train_size}{
This argument allows the user to select the perentage (in the form of a decimal) of the input dataframe observations that will go towards the training data.
}
}

\value{
This function returns a list of four objects: \code{X_train, y_train, X_test, and y_test}. \code{X_train} and \code{X_test} are in the form of dataframes and \code{y_train} and \code{y_test} are vectors.
}
\references{
Max Kuhn, author of the \code{caret} package

http://topepo.github.io/caret/data-splitting.html

Hyndman and Athanasopoulos (2013), Forecasting: principles and practice. https://otexts.com/fpp2/
}
\author{
Nousra Chaibati, Naomi Kaldjob & Joseph Pelham
Max Kuhn, author of the \code{caret} package
}

\examples{
heart <-naivebayes2.0::heart
ML_list <- train_test_split(heart, "HeartDisease",train_size = .8)
ML_list$X_train
ML_list$X_test
ML_list$y_train
ML_list$y_test
}

