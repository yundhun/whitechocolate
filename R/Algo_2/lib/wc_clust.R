clust_stocks <- function(dat,n){

  rm_cols <- nearZeroVar(dat)
  if(length(rm_cols)!=0){
    dat <- dat[,-nearZeroVar(dat)]
  }
  rm_cols = c()
  for(x in 1:ncol(dat)){
    if(  any(is.na(dat[,x])) ){
      rm_cols <- c(rm_cols,x)
    }
  }
  if(length(rm_cols)>0) dat <- dat[,-rm_cols]
  
  clust2 <- ClustOfVar::kmeansvar(X.quanti = dat, init=n)
  clust2 <- clust2$var
  clust <- list()
  ii <- 1
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
    
    plot_yn <- FALSE
    ts <- 1
    selectedStock <- NULL
    for(ct in rownames(cts)){
      if(cor(dat[,ct], tmp) < 0.9) next()
      if(plot_yn){
        if(ts==1){
          plot(nor(dat[,ct]),type='l',col='gray', main=i)
          points(nor(tmp), type='l')
        }else{
          points(nor(dat[,ct]),type='l',col='gray')
        }
        ts <- ts+1
      }
      
      selectedStock <- c(selectedStock,ct)
    }
    
    if( !is.null(selectedStock) && length(selectedStock) > 1) {
      clust[[ii]] <- selectedStock
      ii <- ii + 1 
    }
  }
  return(clust)
}

nor <- function(x){ (x-min(x))/(max(x)-min(x)) }