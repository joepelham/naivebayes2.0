\name{calc_accuracy}
\alias{calc_accuracy}

\title{
Model Evaluation: Calculate the Accuracy Score
}
\description{
Calculate the accuracy score to evaluate a given model.
}
\usage{
calc_accuracy(actual, predicted)
}

\arguments{
  \item{actual}{
A vector from the test data representing the actual values of the predicted variable (\code{y_test})
}
  \item{predicted}{
A vector of predicted values produced by a machine learning model.
}
}

\details{
This function calculates accuracy score by dividing the number of correct predictions by the sum of the total predictions.
}
\value{
This function returns a single value representing the accuracy score.
}

\author{
Nousra Chaibati, Naomi Kaldjob & Joseph Pelham
}
\note{
This function is utilized in the \code{test} method of the \code{naivebayes} R6 object. If used outside of this object, both \code{actual} and \code{predicted} values must be entered.
}

\examples{
predicted = c("A","B","A","B","A","B","A","B")
actual = c("B","A","A","B","A","A","A","B")

calc_accuracy(actual, predicted)

}
