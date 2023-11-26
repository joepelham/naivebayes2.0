\name{calc_recall}
\alias{calc_recall}

\title{
Model Evaluation: Calculate the Recall Scores
}
\description{
This function produces the recall scores for each class of the variable as well as the Macro Average Recall score
}
\usage{
calc_recall(actual, predicted)
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
This function calculates recall for each class by dividing the number of 'true positives' by the number of 'true positives' plus the number of 'false negatives.' Then, to calculate the macro average recall score, the function simply takes the mean of the class recall scores.
}
\value{
The function returns a list with the Macro Recall Score and the vector of class recall scores.
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

calc_recall(actual, predicted)

}
