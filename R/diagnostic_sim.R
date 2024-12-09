diagnostic_sim<-function(x,lower_lambda,upper_lambda,inc){
  ##Run simulate diagnostic summary on vector x, over the range(lower_lamba,
  ##upper_lambda), and increment the lambda value by the value specified
  lambda_range<- seq(lower_lambda,upper_lambda,by=inc)
  if (any(x <= 0)) {
    stop("All values in x must be positive for the Box-Cox transformation.")
  }
  boxCoxTransform <- function(x, lambda) {
    if (lambda == 0) {
      return(log(x))
    } else {
      return((x^lambda - 1) / lambda)
    }
  }
  summary_box<-data.frame(
    Lambda=lambda_range,
    Skewness= numeric(length(lambda_range)),
    Kurtosis=numeric(length(lambda_range))
  )

  for(i in seq_along(lambda_range)){
    lambda<-lambda_range[i]
    transformed<-boxCoxTransform(x,lambda)
    summary_box$Skewness[i]<- e1071::skewness(transformed)
    summary_box$Kurtosis[i]<-e1071::kurtosis(transformed)
  }
  return(summary_box)
}

x<- rchisq(100,3)
hist(x)
diagnostic_sim(x,-2,2,0.1)

