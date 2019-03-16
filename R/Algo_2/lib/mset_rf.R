source("lib/bootlimit.R")
library(MASS)
library(rpart)
mset_DTregress = function(trdat, tedat, alpha) {
  
  ## training
  y_hat_tr = matrix(numeric(0), nrow(trdat),ncol(trdat) ) 
  y_hat_ts = matrix(numeric(0), nrow(tedat),ncol(tedat) ) 
  resid_mat_tr = matrix(numeric(0), nrow(trdat),ncol(trdat) )
  resid_mat_ts = matrix(numeric(0), nrow(tedat),ncol(tedat) )
  
  
  
  for ( i in 1:ncol(trdat) ) {
    temp_m = rpart(trdat[,i]~., data = trdat[,-i])
    y_hat_tr[,i] = predict(temp_m,trdat)
    resid_mat_tr[,i] = trdat[,i] - y_hat_tr[,i];
  }
  
  ## control limit
  ucl = matrix(0,1,ncol(resid_mat_tr))
  lcl = matrix(0,1,ncol(resid_mat_tr))
  
  for (i in 1:ncol(trdat)){
    ucl[,i] = bootlimit(na.approx(resid_mat_tr[,i]), alpha, 100)
    lcl[,i] = bootlimit(na.approx(resid_mat_tr[,i]), 1-alpha, 100)
  }
  
  for ( i in 1:ncol(tedat) ) {
    y_hat_ts[,i] = predict(temp_m,tedat)
    resid_mat_ts[,i] = tedat[,i] - y_hat_ts[,i];
  }
  
  ret <- list(
    predictValue = y_hat_ts,
    resid_tr = resid_mat_tr, 
    resid_ts = resid_mat_ts,
    ucl = ucl,
    lcl = lcl
  )
  
  ## testing
  return(ret)
}
