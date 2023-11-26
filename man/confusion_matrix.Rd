\name{confusion_matrix}
\alias{confusion_matrix}

\title{
Model Evaluation: Calculate the Confusion Matrix
}
\description{
This simple function produces a confusion matrix to compare actual and predicted values.
}
\usage{
confusion_matrix(actual, predicted)
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
This function takes the two vectors, ensures that they are of equal length, and returns a table.
}
\value{
The function returns a confusion matrix in the form of a table.
}

\author{
Nousra Chaibati, Naomi Kaldjob & Joseph Pelham
}
\note{
This function is utilized in the \code{test} method of the \code{naivebayes} R6 object. If used outside of this object, both \code{actual} and \code{predicted} values must be entered
}


\examples{
predicted = c("A","B","A","B","A","B","A","B")
actual = c("B","A","A","B","A","A","A","B")

confusion_matrix(actual, predicted)

}

