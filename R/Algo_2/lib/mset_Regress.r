library(pracma)
library(randomForest)

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
    predict_mat[,i] <- (new_dat[,i] - pred) / new_dat[,i]
  }
  
  ret <- list(
    predict = predict_mat
  )
  return(ret)
}




mset_Regress_RF = function(x,predict_period) {
  
  predict_mat = matrix(numeric(0), predict_period,ncol(x) ) 
  predict_mat2 = matrix(numeric(0), predict_period,ncol(x) ) 
  colnames(predict_mat) <- colnames(x)
  colnames(predict_mat2) <- colnames(x)
  colNms <- colnames(predict_mat)
  
  tr_dat <- x[1:(nrow(x)-predict_period),]
  new_dat <- x[(nrow(x)-predict_period+1):nrow(x),]
  
  for( i in 1:ncol(x)){
    model = randomForest(formula(paste(colNms[i]," ~ .",sep='')),
                  data=tr_dat,ntree=10)
  
    pred <- predict(model, newdata = new_dat)
    predict_mat[,i] <- (new_dat[,i] - pred) / new_dat[,i]
    predict_mat2[,i]<- pred
  }
  
  ret <- list(
    predict = predict_mat,
    predict2 = predict_mat2
  )
  return(ret)
}


