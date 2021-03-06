% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mape.R
\name{mape}
\alias{mape}
\title{Mean Absolute Percentage Error}
\usage{
mape(actual, predicted, w = NULL, ...)
}
\arguments{
\item{actual}{Strictly positive observed values.}

\item{predicted}{Predicted values.}

\item{w}{Optional case weights.}

\item{...}{Further arguments passed to \code{weighted_mean}.}
}
\value{
A numeric vector of length one.
}
\description{
Calculates weighted mean absolute percentage error of predicted values. The smaller, the better.
}
\examples{
mape(1:10, c(1:9, 12))
mape(1:10, c(1:9, 12), w = 1:10)
}
