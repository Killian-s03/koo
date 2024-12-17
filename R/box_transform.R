#' Box Cox Transformation
#'
#' This Function Applies the box-cox transformation to a numeric vector
#' The Box-Cox transformation is used to stabilize variance and make data more normally distributed.
#' If lambda is 0, a log transformation is applied to the data
#'
#' @param x A numeric vector to be transformed. All values of x must be positive and must be numeric
#' @param lambda Transformation parameter. If this is 0 a log transformation is applied. Must be in the range \code{[-5, 5]}
#'
#' @author Killian Slater
#'
#' @return A numeric vector of the transformed values
#' @examples
#' x<-c(1,2,3,4,10,15,25)
#' box_transform(x,lambda=0.5)
#' box_transform(x,lambda=1)
#'
#'@export
box_transform <-function(x,lambda){
  if(any(x<=0)){
    stop("All x values must be positive")
  }
  if (!all(is.numeric(x))){
    stop("Input Parameters must be all numeric")
  }
  if(lambda< (-5)|lambda>5){
    stop("Value for lambda must be in the range [-5,5]. This ensures optimal transformation")
  }

  if(lambda==0){
    return(log(x))
  }else{
    return(((x^lambda)-1)/lambda)
  }
}




