\name{discretize}
\alias{discretize}

\title{
Discretization of Numeric Data
}
\description{
This function discretizes numeric data in a dataframe using the mdlp() function from the discretization package.
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
A list of column names for predictor variables intended for use in the modelization. When set to predictors = 'all', the default setting, all variables other than the input target variable will be entered. Non-numeric columns will automatically be excluded from the discretization process but reattached in the dataframe returned by the function.
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns a dataframe with discretized versions of the numeric variables from the input dataframe. Note that the function replaces the numeric data. Therefore, it is recommended to create an object with a new name if you want to avoid erasing the original numeric data.
}
\references{
The discretization library
}
\author{
Nousra Chaibati, Naomi Kaldjob & Joseph Pelham
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, target_var)
{
    target <- data[, target_var, drop = F]
    numeric_cols <- sapply(data, is.numeric)
    numeric_data <- data[, numeric_cols, drop = F]
    non_numeric_cols <- !numeric_cols
    non_numeric_data <- data[, non_numeric_cols, drop = F]
    non_numeric_data <- non_numeric_data[, -which(colnames(non_numeric_data) ==
        target_var), drop = FALSE]
    new_data <- data.frame(numeric_data, target)
    disc <- discretization::mdlp(new_data)
    disc <- disc$Disc.data
    final_data <- data.frame(non_numeric_data, disc)
    return(final_data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
