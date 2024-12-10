#' Inverse Box-Cox Transformation
#'
#' A function to compute the inverse Box-Cox transformation for a given transformed dataset `x`
#' and a given lambda value. The transformation returns the original pre-transformation data.
#'
#' @param x Transformed data (numeric vector or matrix).
#' @param lambda Value of lambda used in the original Box-Cox transformation.
#' @return A numeric vector or matrix with the original data restored (inverse transformation).
inverse_boxcox <- function(x, lambda) {
  if (lambda == 0) {
    return(exp(x))
  } else {
    return((x * lambda + 1)^(1 / lambda))
  }
}
