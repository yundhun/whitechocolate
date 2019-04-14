library(pracma)
library(randomForest)
#MSET
mset_Regress = function(x,predict_period) {
  
  predict_mat = matrix(numeric(0), predict_period,ncol(x) ) 
  colnames(predict_mat) <- colnames(x)
  colNms <- colnames(predict_mat)

  predict_mat_2 = matrix(numeric(0), predict_period,ncol(x) ) 
  colnames(predict_mat_2) <- colnames(x)
    
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
    predict_mat_2[,i] <- pred
  }
  
  ret <- list(
    predict = predict_mat,
    predict2 = predict_mat_2
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


#MSET Filter
mset_filter <- function(mset,predict_period,msetRange){
  if(is.na(all(mset))) return(NULL)
  if( round(ncol(mset)*0.8) < 2) return(NULL)
  mset2 <- sort(colSums(abs(mset[1:predict_period,])))[1:max(1, round(ncol(mset)*0.8) )]
  mset <- mset[, names(mset2)]
  # mset_recommend_list <- NULL
  # for( cn in colnames(mset)){
  #   lm_t <- lm(tail(mset[,cn],msetRange)~c(1:msetRange))
  #   if( mean(tail(mset[,cn],msetRange)) < 0 && 
  #       lm_t$coefficients[2] < 0 ){
  #     mset_recommend_list <- c(mset_recommend_list,cn)
  #   }
  # }
  mset_recommend_list <- colnames(mset[,colMeans(tail(mset,msetRange)) < 0]) 
  return(mset_recommend_list)
}