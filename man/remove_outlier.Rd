% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RemoveOutlier.r
\name{remove_outlier}
\alias{remove_outlier}
\title{Remove Outlier}
\usage{
remove_outlier(data, max_perc = 0.99, min_perc = 0.91, threshold = 5,
  col_name = "", na.rm = TRUE, ...)
}
\arguments{
\item{data}{Any Data Set}

\item{max_perc}{Maximum percentile limit for considering as outlier, default is 0.99}

\item{min_perc}{Minimum percentile limit for considering as outlier, default is 0.91}

\item{threshold}{No of Variable that should occur simultaneoulsy as outliers, default is 5}

\item{col_name}{Vector with column names to be considered for outlier, default is "" and considers all variable for outlier}

\item{na.rm}{logical, Remove NA values while calculating percentile, default is TRUE}

\item{...}{Other Parameters for quantile function}
}
\value{
Data set with no set of outliers based on percentile
}
\description{
Remove Outlier Rows
}
\details{
Gives Data Set with reduced no of rows based on simple criteria that
outliers will have either highest or lowest percentile. It first calculates
all the percentile for each variable and then based on threshold, remove the
outliers.
}
\examples{
data(mtcars)
mtcars_1 <- remove_outlier(iris,threshold = 1)
mtcars_2 <- remove_outlier(iris,threshold = 2)
mtcars_3 <- remove_outlier(iris,threshold = 3)
# check the no of rows removed and at various thresholds
dim(mtcars)
dim(mtcars_1)
dim(mtcars_2)
dim(mtcars_3)
}
\author{
Jitin Kapila
}

