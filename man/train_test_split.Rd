\name{train_test_split}
\alias{train_test_split}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Train/Test Split
}
\description{
Using the function createDataPartition from the caret package, this function takes a dataframe, the name of the target variable (y), the names of predictor varaibles, and the desired size for the training set and returns a list of four objects (X_train, y_train, X_test, and y_test) ready for the use in the calculation of Naive Bayes or other machine learning algorithms.
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
A list of column names for predictor variables intended for use in the modelization. When set to predictors = 'all', the default setting, all variables other than the input target variable will be entered. Non-numeric columns will automatically be excluded from the discretization process but reattached in the dataframe returned by the function.
}
  \item{train_size}{
This argument allows the user to select the perentage (in the form of a decimal) of the input dataframe observations that will go towards the training data.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
function (x)
{
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
