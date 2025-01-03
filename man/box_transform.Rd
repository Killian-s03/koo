% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box_transform.R
\name{box_transform}
\alias{box_transform}
\title{Box Cox Transformation}
\usage{
box_transform(x, lambda)
}
\arguments{
\item{x}{A numeric vector to be transformed. All values of x must be positive and must be numeric}

\item{lambda}{Transformation parameter. If this is 0 a log transformation is applied. Must be in the range \code{[-5, 5]}}
}
\value{
A numeric vector of the transformed values
}
\description{
This Function Applies the box-cox transformation to a numeric vector
The Box-Cox transformation is used to stabilize variance and make data more normally distributed.
If lambda is 0, a log transformation is applied to the data
}
\examples{
x<-c(1,2,3,4,10,15,25)
box_transform(x,lambda=0.5)
box_transform(x,lambda=1)

}
\author{
Killian Slater
}
