#' Inverse Box-Cox Transformation
#'
#' A function to compute the inverse Box-Cox transformation for a given transformed dataset `x`
#' and a given lambda value. The transformation returns the original pre-transformation data.
#'
#' @param x Transformed data (numeric vector or matrix).
#' @param lambda Value of lambda used in the original Box-Cox transformation.
#' @return A numeric vector or matrix with the original data restored (inverse transformation).
#' @example
#' inverse_boxcox(c(0, 1, 2), 0.5)

inverse_boxcox <- function(x, lambda) {
  if (lambda == 0) {
    x |> exp()
  } else {
    x |> (\(y) (y * lambda + 1)^(1 / lambda))()
  }
}
