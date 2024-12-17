\name{box_plot}
\title{Box-Cox Transformation Visualization}
\alias{box_plot}
\description{
  This function compares the distribution of the original data with the transformed data
  using side-by-side histograms or density plots.
}
\usage{
box_plot(x, transformed_x = NULL, lambda = NULL, plot_type = "histogram")
}
\arguments{
  \item{x}{A numeric vector of the original data.}
  \item{transformed_x}{An optional numeric vector of the transformed data. If not provided, the function will calculate it.}
  \item{lambda}{Transformation parameter for the Box-Cox transformation (ignored if `transformed_x` is provided).}
  \item{plot_type}{The type of plot to generate: "scatter" (default), "histogram", or "density".}

  Errors will be omitted in the following cases:
  If all values of x are not positive
  If all values of x are not numeric
  If transformed_x is not provided, lambda must be specified.
  If a graph is specified but isn't one of the followin, 'scatter', 'histogram' or 'density'
}
\details{
  This function creates visualizations to compare the original and transformed data. It can generate scatter plots,
  histograms, or density plots based on the input parameters.
}
\value{
  A ggplot2 object displaying the comparison of original and transformed data.
}
\examples{
x <- c(1, 2, 3, 4, 10, 15, 25)
box_plot(x, lambda = 0.5)
}
\author{
Olivia Summerville
}
