box_transform <-function(x,lambda){
  if(any(x<=0)){
    stop("All x values must be positive")
  }

  if(lambda==0){
    return(log(x))
  }else{
    return(((x^lambda)-1)/lambda)
  }
}



