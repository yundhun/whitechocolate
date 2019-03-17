library(caret)

#MSET Filter
mset_filter <- function(mset,predict_period,msetRange){
  if(is.na(all(mset))) next()
  if( round(ncol(mset)*0.8) < 2) next()
  mset2 <- sort(colSums(abs(mset[1:predict_period,])))[1:max(1, round(ncol(mset)*0.8) )]
  mset <- mset[, names(mset2)]
  colnames(mset)
  mset_recommend_list <- colnames(mset[,colMeans(tail(mset,msetRange)) < 0]) 
  return(mset_recommend_list)
}

dat_prep_1 <- function(dat){
  dat <- as.data.frame(dat)
  
  colnames(dat) <- paste0("X",colnames(dat))
  dat <- dat[,-nearZeroVar(dat)]
  
  rm_cols = c()
  for(x in 1:ncol(dat)){
    if(  any(is.na(dat[,x])) ){
      rm_cols <- c(rm_cols,x)
    }
  }
  if(length(rm_cols)>0) dat <- dat[,-rm_cols]
  
  colnms <- colnames(dat)
  colnms <- gsub(" ", "", colnms, fixed = TRUE)
  colnms <- gsub("&", "", colnms, fixed = TRUE)
  colnms <- gsub("-", "", colnms, fixed = TRUE)
  colnames(dat) <- colnms
  
  return(dat)
}

nor <- function(x){ (x-min(x))/(max(x)-min(x)) }

clust_stocks <- function(dat,n){
  clust2 <- ClustOfVar::kmeansvar(X.quanti = dat, init=n)
  clust2 <- clust2$var
  clust <- list()
  for( i in 1:length(clust2)){
    cts <- clust2[[i]]
    if(length(cts) == 2) next()
    cts <- as.data.frame(cts)
    
    tmp <- rep(0,nrow(dat))
    for(ct in rownames(cts)){
      tmp <- tmp + nor(dat[,ct])
      if(is.na(tmp)[1]){
        print(ct)
        break
      }
    }
    
    ts <- 1
    selectedStock <- NULL
    for(ct in rownames(cts)){
      if(cor(dat[,ct], tmp) < 0.9) next()
      if(ts==1){
        plot(nor(dat[,ct]),type='l',col='gray', main=i)
        points(nor(tmp), type='l')
      }else{
        points(nor(dat[,ct]),type='l',col='gray')
      }
      ts <- ts+1
      selectedStock <- c(selectedStock,ct)
    }
    clust[[i]] <- selectedStock
  }
  return(clust)
}