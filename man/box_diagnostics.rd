\name{box_diagnostics}
\title{Box-Cox Diagnostic Plots}
\description{
  This function generates diagnostic plots comparing the original data and the transformed data.
  Includes QQ plots for normality checks and a box plot to highlight change in data spread and central tendency.
}
\usage{
box_diagnostics(x, transformed_x = NULL, lambda = NULL)
}
\arguments{
  \item{x}{A numeric vector of the original data.}
  \item{transformed_x}{An optional numeric vector of the transformed data. If not provided, the function will calculate it using the Box-Cox transformation.}
  \item{lambda}{Transformation parameter for the Box-Cox transformation (ignored if `transformed_x` is provided).}

  Errors will be omitted in the following cases:
  If all values of x are not positive
  If all values of x are not numeric
  If transformed_x is not provided, lambda must be specified.
}
\details{
  This function generates a combined grid of diagnostic plots comparing the original and transformed data:
  \itemize{
    \item QQ plots for normality checks on both the original and transformed data.
    \item A box plot comparing the distribution of the original and transformed data to highlight changes in spread and central tendency.
  }
}
\value{
  A combined grid of diagnostic plots (QQ plots and a box plot) for the original and transformed data.
}
\examples{
x <- c(1, 2, 3, 4, 10, 15, 25)
box_diagnostics(x, lambda = 0.5)
}
\author{Olivia Summerville}
\import{ggplot2}
\import{gridExtra}
