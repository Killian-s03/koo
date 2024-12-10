#' Summary of Box Cox Diagnostics
#'
#' This function takes in a numeric vector, a range of lambda values and a value to increment the range
#' The Box-Cox transformation is applied to the data and a diagnostic summary is outputted.
#' The summary includes: Lambda value
#'                       Skewness value
#'                       Kurtosis value (tailedness)
#'                       Shapiro Wilk P-value (test for normality)
#' For each value of lambda
#'
#' @param x A numeric vector
#' @param lower_lambda a value for the lower range of lambda
#' @param upper_lambda a value for the upper range of lambda
#' @param inc a value for incrementing the range of lambda for each output across the range
#' @param summary_box a data frame for storing the summary output
#'
#' Errors will be omitted in the following cases:
#'        If all values of x are not positive
#'        If all values of x are not numeric
#'        If the range of lambda values are not within [-5,5]. An optimal value will be in this range
#'        If increment value is less than or equal to 0
#'        If input vector contains any NA values
#' @author Killian Slater
#'
#'
#' @return A data frame containing a summary of all diagnostics from each lambda value
#'
#' @import e1017
#' @importFrom stats shapiro.test
#'
#'
#' @examples
#' x<-c(1,2,3,4,10,15,25)
#' diagnostic_sim(x,lower_lambda=-2,upper_lambda=2,inc= 0.5)
#'
#'
#' @export
diagnostic_sim<-function(x,lower_lambda=-2,upper_lambda=2,inc=1){
  if (any(x <= 0)) {
    stop("All values in x must be positive for the Box-Cox transformation.")
  }

if (!all(is.numeric(x))){
  stop("Input Parameters must be all numeric")
}

if(lower_lambda<(-5) & upper_lambda>5| lower_lambda< (-5)|upper_lambda>5){
  stop("Value for λ must be in the range [-5,5]")
}
if(anyNA(x)){
  stop("Input vector 'x' contains missing values. Please remove them before proceeding" )
}

if(inc<=0){
  stop("Increment value 'inc' must be positive")
}
  lambda_range<- seq(lower_lambda,upper_lambda,by=inc)
  summary_box<-data.frame(
    Lambda=lambda_range,
    Skewness= numeric(length(lambda_range)),
    Kurtosis=numeric(length(lambda_range)),
    Shapiro_Wilk_P_Value=numeric(length(lambda_range))
  )

  for(i in seq_along(lambda_range)){
    lambda<-lambda_range[i]
    transformed<-box_transform(x,lambda)
    summary_box$Skewness[i]<- e1071::skewness(transformed)
    summary_box$Kurtosis[i]<-e1071::kurtosis(transformed)
    summary_box$Shapiro_Wilk_P_Value[i]<-shapiro.test(transformed)$p.value
  }
  return(summary_box)
}

