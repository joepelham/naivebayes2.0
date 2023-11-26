\name{calc_f1}
\alias{calc_f1}

\title{
Model Evaluation: Calculate the F1 (or F beta) Score
}
\description{
This function calculates the F1 (or F beta) score to assess the prediction quality of a given model.
}
\usage{
calc_f1(precision, recall, beta = 1)
}

\arguments{
  \item{precision}{
The precision score for a given model, calculated with the \code{calc_precision} function from this package or using another method
}
  \item{recall}{
The recall score for a given model, calculated with the \code{calc_recall} function from this package or using another method
}
  \item{beta}{
With a default value of 1 (to calculate the "F1" score), changing \code{beta} allows the user to adjust the balance between precision and recall based on the specific requirements of your application.
}
}
\details{
The function uses the following formula to calculate the F1/F Beta score:

\code{(1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)}
}

\value{
This function returns a single value representing the F1 (or F beta) score.

}

\author{
Nousra Chaibati, Naomi Kaldjob & Joseph Pelham
}
\note{
This function is utilized in the \code{test} method of the \code{naivebayes} R6 object. If used outside of this object, both \code{actual} and \code{predicted} values must be entered
}

\examples{
recall = .81
precision = .85

calc_f1(precision,recall)

}

