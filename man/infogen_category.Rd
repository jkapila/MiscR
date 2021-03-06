% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InfoCategory.r
\name{infogen_category}
\alias{infogen_category}
\title{Category / Descrete Information}
\usage{
infogen_category(data)
}
\arguments{
\item{data}{Any Data Set}
}
\value{
Information about Categorical / Descrete Variables
}
\description{
Gives all information about categorical / descrete variable in data.
}
\details{
Gives Variable, Level, Frequency, Percentage
}
\examples{
data(iris)
infogen_category(iris)

data(mtcars)
info <- infogen_category(mtcars)
print(info)
}
\author{
Jitin Kapila
}

