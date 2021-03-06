% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blblogreg.R
\name{confint.blblog}
\alias{confint.blblog}
\title{Obtaining Confidence Intervals}
\usage{
\method{confint}{blblog}(object, parm = NULL, level = 0.95, par = FALSE, ...)
}
\arguments{
\item{object}{object of class "blblog"}

\item{parm}{NULL by default, as it takes the variables obtained from the blblog fit. This should be a vector of characters that user wants to find confidence interval on.}

\item{level}{numeric. Specify the confidence level}

\item{par}{logical. Indicate whether or not user wants parallelization to be run. FALSE by default.}

\item{...}{optional arguments to be passed}
}
\value{
an interval for the specified confidence level for the variables involved.
}
\description{
We use this function \code{confint} in order to find the confidence interval for all variables involved.
}
\details{
\code{confint} produces the confidence intervals for the coefficients for the fitted bag of little bootstraps algorithm for logistic regression.
}
\examples{
fit <- blblog(Species ~ Sepal.Length, data = iris, m = 3, B = 100)
confint(fit)
}
