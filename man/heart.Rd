\name{heart}
\alias{heart}
\docType{data}
\title{
Heart Disease Prediction Dataset
}
\description{
This dataset from the UCI Machine Learning Repository combines five existing heart related datasets with common features. It has 12 variables and 918 observations. The variable \code{HeartDisease} is a perfect dependent variable for examining machine learning classification techniques.
}
\usage{data("heart")}
\format{
  A data frame with 918 observations on the following 12 variables.
  \describe{
    \item{\code{Age}}{a numeric vector} [years]
    \item{\code{Sex}}{a character vector} [M: Male, F: Female]
    \item{\code{ChestPainType}}{a chatacter vector} [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]
    \item{\code{RestingBP}}{a numeric vector} [mm Hg]
    \item{\code{Cholesterol}}{a numeric vector} [mm/dl]
    \item{\code{FastingBS}}{a numeric vector} [1: if FastingBS > 120 mg/dl, 0: otherwise]
    \item{\code{RestingECG}}{a character vector} [Normal: Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria]
    \item{\code{MaxHR}}{a numeric vector} [Numeric value between 60 and 202]
    \item{\code{ExerciseAngina}}{a character vector} [Y: Yes, N: No]
    \item{\code{Oldpeak}}{a numeric vector} [Numeric value measured in depression]
    \item{\code{ST_Slope}}{a character vector} [Up: upsloping, Flat: flat, Down: downsloping]
    \item{\code{HeartDisease}}{a character vector} [1: heart disease, 0: Normal]
  }
}

\source{
This data was taken from kaggle.com, where the following information about its source are provided:

"This dataset was created by combining different datasets already available independently but not combined before. In this dataset, 5 heart datasets are combined over 11 common features which makes it the largest heart disease dataset available so far for research purposes. The five datasets used for its curation are:

Cleveland: 303 observations
Hungarian: 294 observations
Switzerland: 123 observations
Long Beach VA: 200 observations
Stalog (Heart) Data Set: 270 observations
Total: 1190 observations
Duplicated: 272 observations

Final dataset: 918 observations

Every dataset used can be found under the Index of heart disease datasets from UCI Machine Learning Repository on the following link: https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/"
}
\references{
fedesoriano. (September 2021). Heart Failure Prediction Dataset. Retrieved [Date Retrieved] from https://www.kaggle.com/fedesoriano/heart-failure-prediction.
}
\examples{
data(heart)
data("heart")
}
\keyword{datasets}
