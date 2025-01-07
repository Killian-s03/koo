#' Estimate Optimal Lambda for Box-Cox Transformation
#'
#' This method estimates the optimal value of lambda for a Box-Cox transformation directly from a data frame or matrix.
#' The method calculates log-likelihood values for a range of lambda values and selects the optimal lambda.
#'@param object An object for which the optimal lambda is estimated.
#'@param ... Additional arguements passed to specific methods
#' @export
estimate_optimal_lambda <- function(object, ...) {
  UseMethod("estimate_optimal_lambda")
}

#' objects of class data.frame
#' @rdname estimate_optimal_lambda
#' @param object An object (data.frame) containing the response variable and predictors.
#' @param lambda_range A numeric vector of length 2 specifying the range of lambda values to consider (default: -2 to 2).
#' @param resolution The number of lambda values to evaluate within the range (default: 100).
#' @param ... Additional arguements passed to specific methods
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
estimate_optimal_lambda.data.frame <- function(object,..., lambda_range = c(-2, 2), resolution = 100) {
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
#' @rdname estimate_optimal_lambda
#' @param object An object (matrix) containing the response variable and predictors.
#' @param lambda_range A numeric vector of length 2 specifying the range of lambda values to consider (default: -2 to 2).
#' @param resolution The number of lambda values to evaluate within the range (default: 100).
#' @param ... Additional arguements passed to specific methods
#' @export
estimate_optimal_lambda.matrix <- function(object,..., lambda_range = c(-2, 2), resolution = 100) {
  # ensure matrix has a response column and predictors
  if (ncol(object) < 2) {
    stop("Matrix must have at least two columns (response variable + predictors).")
  }

  response <- object[, 1]  #assumes the first column is the response variable
  predictors <- object[, -1, drop = FALSE]

  # call shared calculation function
  return(calculate_optimal_lambda(response, predictors, lambda_range, resolution))
}



#'Method for objects of class numeric
#'@rdname estimate_optimal_lambda
#'@param object Object of type numeric vector
#' @param lambda_range A numeric vector of length 2 specifying the range of lambda values to consider (default: -2 to 2).
#' @param resolution The number of lambda values to evaluate within the range (default: 100).
#' @param ... Additional arguements passed to specific methods

#'
#' @export
estimate_optimal_lambda.numeric <- function(object,..., lambda_range = c(-2, 2), resolution = 100) {
  if (any(object <= 0)) {
    stop("All elements of the numeric vector must be strictly positive for the Box-Cox transformation.")
  }
  df <- data.frame(y = object)
  predictors <- NULL
  estimate_optimal_lambda.data.frame(df, lambda_range = lambda_range,resolution = resolution)
}




#' Shared function for calculating optimal lambda
#' @rdname estimate_optimal_lambda
#' @param lambda_range A numeric vector of length 2 specifying the range of lambda values to consider (default: -2 to 2).
#' @param resolution The number of lambda values to evaluate within the range (default: 100).
#' @param response Response variable from object for the fitted model
#' @param predictors predictors from the model
#' @export
calculate_optimal_lambda <- function(response, predictors, lambda_range=c(-2,2), resolution=100) {
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
    model_trans <- stats::lm(y_trans ~ ., data = as.data.frame(cbind(y_trans, predictors)))
    stats::logLik(model_trans)
  })

  #select lambda that maximizes log-likelihood
  optimal_lambda <- lambdas[which.max(log_likelihoods)]

  return(optimal_lambda)
}

#'default method
#'@rdname estimate_optimal_lambda
#'@param object an object for the default lambda estimation
#'@param ... additional parameters
#' @export
estimate_optimal_lambda.default <- function(object, ...) {
  stop("No method implemented for this object class.")
}

