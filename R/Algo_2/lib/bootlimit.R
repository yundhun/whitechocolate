library(bootstrap);
bootlimit <- function(stat, alpha, n){
  
  theta <- function(x)
  {
    quantile(x, 1-alpha);      ## 0.05<-percentile 
  }
  
  result <- bootstrap(as.matrix(stat), n, theta);
  
  value = mean(result$thetastar);
  
  ret <- value
  
  ## testing
  return(ret)
}
