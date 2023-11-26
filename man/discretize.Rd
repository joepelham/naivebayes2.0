\name{discretize}
\alias{discretize}

\title{
Discretization of Numeric Data using the Minimum Description Length Principle
}

\description{
This function discretizes numeric data in a dataframe using the \code{mdlp()} function from the \code{discretization} package.
}

\usage{
discretize(data_frame, target_var, predictors = 'all')
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
}

\value{
This function returns a dataframe with discretized versions of the numeric variables from the input dataframe. Note that the function replaces the numeric data. Therefore, it is recommended to create an object with a new name if you want to avoid erasing the original numeric data.
}

\references{
Fayyad, U. M. and Irani, K. B.(1993). Multi-interval discretization of continuous-valued attributes for classification learning, Artificial intelligence, 13, 1022–1027.
}

\author{
Nousra Chaibati, Naomi Kaldjob & Joseph Pelham

- HyunJi Kim (author of \code{discretization} package)
}

\examples{
heart <-naivebayes2.0::heart
discdta <- discretize(heart, "HeartDisease")
}



