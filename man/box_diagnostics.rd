% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box_diagnostics.R
\name{box_diagnostics}
\alias{box_diagnostics}
\title{Box-Cox Diagnostic Plots}
\usage{
box_diagnostics(x, transformed_x = NULL, lambda = NULL)
}
\arguments{
\item{x}{A numeric vector of the original data.}

\item{transformed_x}{An optional numeric vector of the transformed data. If not provided, the function will calculate it.}

\item{lambda}{Transformation parameter for the Box-Cox transformation (ignored if \code{transformed_x} is provided).}
}
\value{
A combined grid of diagnostic plots.
}
\description{
This function generates diagnostic plots comparing the original data and the transformed data.
Includes QQ plots for normality checks and a box plot to highlight change in data spread and central tendecny
}
\examples{
x <- c(1, 2, 3, 4, 10, 15, 25)
box_diagnostics(x, lambda = 0.5)
}
\author{
Olivia Summerville
}
