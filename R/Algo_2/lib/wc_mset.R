library(pracma)

#MSET
mset_Regress = function(x,predict_period) {
  
  predict_mat = matrix(numeric(0), predict_period,ncol(x) ) 
  colnames(predict_mat) <- colnames(x)
  colNms <- colnames(predict_mat)
  
  tr_dat <- x[1:(nrow(x)-predict_period),]
  new_dat <- x[(nrow(x)-predict_period+1):nrow(x),]
  
  for( i in 1:ncol(x)){
    model <- lm(
      formula(paste(colNms[i]," ~ .",sep='')),
      data=tr_dat
    )
    #model <- step(model, trace = F)
    pred <- predict(model, newdata = new_dat)
    predict_mat[,i] <- scale(new_dat[,i] - pred)
  }
  
  ret <- list(
    predict = predict_mat
  )
  return(ret)
}

#MSET Filter
mset_filter <- function(mset,predict_period,msetRange){
  if(is.na(all(mset))) return(NULL)
  if( round(ncol(mset)*0.8) < 2) return(NULL)
  mset2 <- sort(colSums(abs(mset[1:predict_period,])))[1:max(1, round(ncol(mset)*0.8) )]
  mset <- mset[, names(mset2)]
  colnames(mset)
  mset_recommend_list <- colnames(mset[,colMeans(tail(mset,msetRange)) < 0]) 
  return(mset_recommend_list)
}