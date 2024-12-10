#' Estimate Optimal Lambda for Box-Cox Transformation
#'
#' This method estimates the optimal value of lambda (λ) for a Box-Cox transformation directly from a data frame or matrix.
#' The method calculates log-likelihood values for a range of λ values and selects the optimal λ.
#'
#' @param object An object (data.frame or matrix) containing the response variable and predictors.
#' @param lambda_range A numeric vector of length 2 specifying the range of λ values to consider (default: -2 to 2).
#' @param resolution The number of λ values to evaluate within the range (default: 100).
#' @return A numeric value representing the optimal lambda.
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   y = rexp(100, rate = 0.2),
#'   x1 = runif(100, 1, 10),
#'   x2 = runif(100, 5, 15)
#'  )
#'  lambda_optimal_df <- estimate_optimal_lambda(df, lambda_range = c(-2, 2))
#'  print(lambda_optimal_df)
#' @export
estimate_optimal_lambda <- function(object, ...) {
  UseMethod("estimate_optimal_lambda")
}

#' objects of class data.frame
#' @export
estimate_optimal_lambda.data.frame <- function(object, lambda_range = c(-2, 2), resolution = 100) {
  #extract response and predictors
  if (!"y" %in% colnames(object)) {
    stop("Response variable 'y' must be present in the data frame.")
  }
  response <- object$y
  predictors <- object[, !colnames(object) %in% "y", drop = FALSE]

  #shared calculation function
  return(calculate_optimal_lambda(response, predictors, lambda_range, resolution))
}

#' Method for objects of class matrix
#' @export
estimate_optimal_lambda.matrix <- function(object, lambda_range = c(-2, 2), resolution = 100) {
  # ensure matrix has a response column and predictors
  if (ncol(object) < 2) {
    stop("Matrix must have at least two columns (response variable + predictors).")
  }

  response <- object[, 1]  #assumes the first column is the response variable
  predictors <- object[, -1, drop = FALSE]

  # call shared calculation function
  return(calculate_optimal_lambda(response, predictors, lambda_range, resolution))
}

#' Shared function for calculating optimal lambda
#' @export
calculate_optimal_lambda <- function(response, predictors, lambda_range, resolution) {
  if (any(response <= 0)) {
    stop("Response variable must contain strictly positive values for Box-Cox transformation.")
  }

  #lambda sequence to evaluate
  lambdas <- seq(lambda_range[1], lambda_range[2], length.out = resolution)

  #log-likelihoods for each lambda
  log_likelihoods <- sapply(lambdas, function(lambda) {
    if (lambda == 0) {
      y_trans <- log(response)
    } else {
      y_trans <- (response^lambda - 1) / lambda
    }

    #fit linear model
    model_trans <- lm(y_trans ~ ., data = as.data.frame(cbind(y_trans, predictors)))
    logLik(model_trans)
  })

  #select lambda that maximizes log-likelihood
  optimal_lambda <- lambdas[which.max(log_likelihoods)]

  return(optimal_lambda)
}

#'default method
#' @export
estimate_optimal_lambda.default <- function(object, ...) {
  stop("No method implemented for this object class.")
}

