library(TTR)
library(qcc)
histlist[which('039240' == names(histlist))]

remove_null_list <- function(x){
  x[sapply(x, is.null)] <- NULL
  x
}

meanfactor=2.5
alpha=0.05
candidate2 <- c()
###거래량 터진시점 가격과 현재시가 비교
for (i in 1:length(histlist)){
  hist <- histlist[[i]][1:40,]
  code <- names(histlist)[i]
  #basevolume <- mean(hist$volume)+sd(hist$volume)*6
  basevolume <- mean(hist$volume)*meanfactor
  
  isthere<-which(hist$volume>=basevolume)
 
  if (length(isthere)>0){
 
    basep<-NULL
    maxp<-hist[which.max(hist$volume),]
    
    islarge <- ((maxp$start_price+maxp$high_price)/2*maxp$volume>5000000000)
    isup <- (maxp$last_price-maxp$start_price>0) 
   
    if(islarge&isup){
      testp = maxp
      while(testp$volume>basevolume){
        rowname <- as.numeric(row.names(testp))
        if((rowname+1)<=nrow(hist)) {
          testp <- hist[(rowname+1),]
        }
        else {
          break
        }
      }
      rowname <- as.numeric(row.names(testp))
      if ((rowname-1)>0){
        basep <- hist[(rowname-1),]
      }else 
      {basep <- testp}
    
      judgea<-abs((basep$start_price-hist[1,]$last_price))/basep$start_price
      if(judgea<=alpha){
      
        candidate2 <<-c(candidate2,code)
      }
    }
  }
  
}


l2<-kosdaq[which(kosdaq$code %in% candidate2),2]

smallhistlist <- histlist[which(names(histlist) %in% candidate2)]

##sma
yoosma<-function(x,n=...){
  price <- rev(x[,2])
  for(i in 1:length(n)){
  mv<-SMA(price,n[i])
  x<-cbind(x,rev(mv),stringsAsFactors=FALSE)
  names(x)[ncol(x)] <- paste0("mv_",n[i])
  }
  x
}
ll<-lapply(smallhistlist,yoosma,n=c(20,60))

##
compare_mv <- function(x,price=...,alpha=...){
  v <- x[1,]$last_price
  cv <- c()
  price_length<- length(price)
  check <- rep(FALSE,price_length)
  for (i in 1:length(price)){
    k <- x[1,which(colnames(x) == price[i])]
    judge <- abs(v-k)/k
    if(judge<=alpha[i]) {
      check[i]<-TRUE
    }
  }
  
  if(all(check)){
    return(TRUE)
  }else{
    
  }
}

compare_mv_new <- function(x,price=...,alpha=...,test="abs"){
  v <- x[1,]$last_price
  cv <- c()
  price_length<- length(price)
  check <- rep(FALSE,price_length)
  for (i in 1:length(price)){
    k <- x[1,which(colnames(x) == price[i])]
    
    judge <- (v-k)/k
    switch(test,
           abs = {
             judge <- abs(judge)
             if(judge<=alpha[i]) {
               check[i]<-TRUE
             }
           },
           low ={
             if( 0>judge & judge >= (-alpha[i])){
               check[i]<-TRUE
             }
           },
           upper = {
             if( (0<= judge & judge <= alpha[i])){
               check[i]<-TRUE
             }
           }
    )
    
  }
  
  if(all(check)){
    return(TRUE)
  }else{
    
  }
}

###거래량 터진시점 가격과 현재시가 비교 5%
kosdaq[which(kosdaq$code %in% candidate2),2]
print("거래량 터진시점 가격과 현재시가 비교 5% 에서 필터링")
##20일선의 5%
t <- sapply(ll,compare_mv,price=c("mv_20"),alpha=0.05)
t20<-remove_null_list(t)
t20list<-kosdaq[which(kosdaq$code %in% names(t20)),2]
print("##20일선의 5%")
t20list
##60일선의 3%
t <- sapply(ll,compare_mv,price=c("mv_60"),alpha=0.03)
t60<-remove_null_list(t)
t60list<-kosdaq[which(kosdaq$code %in% names(t60)),2]
print("##60일선의 3%")
t60list
##20일선의 5% 60일선의 1%
t <- sapply(ll,compare_mv,price=c("mv_20","mv_60"),alpha=c(0.05,0.01))
t2060<-remove_null_list(t)
t2060list<-kosdaq[which(kosdaq$code %in% names(t2060)),2]
print("##20일선의 5% 60일선의 1%")
t2060list


##쌍바닥
t <- sapply(ll,compare_mv_new,price=c("mv_20","mv_60"),alpha=c(0.05,0.01),"upper")
t2060<-remove_null_list(t)
t2060list<-kosdaq[which(kosdaq$code %in% names(t2060)),2]
print("##20일선의 5% 60일선의 1%")
t2060list