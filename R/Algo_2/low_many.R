##기준으로 쌍바닥

counts <- 2
base <- 0.01

maxbase <- 0.05
minbase <- 0.01

candidate2<-sapply(histlist,function(a){
  
  count <- 0

  baseprice <- a[1,]$last_price
  upper <- baseprice*(1+base)
  low <- baseprice*(1-base)
  k<-which(upper >= a$last_price & low <= a$last_price)
  
  if(length(k)>1){
      for (i in 1:(length(k)-1)){
      idx <- k[i]:k[i+1]
      max_price <- max(a[idx,]$last_price)
  
      max_last <- max(baseprice,a[k[i],]$last_price)
      max_price_base <- max_last*(1+maxbase)
      if(max_price_base<=max_price){
        
        if(minbase!=0){
          
          count <- count+1  
        
        }else{
          
          min_price <- min(a[idx,]$last_price)
          judge<- abs(baseprice-min_price)/min_price
          if(judge<=minbase){
            count <- count+1  
          }
            
        }
        
        
      }
    }
  }
  
  count
  
})

candidate2<-names(candidate2[which(candidate2 != 0)])

smallhistlist <- histlist[which(names(histlist) %in% candidate2)]
ll<-lapply(smallhistlist,yoosma,n=c(20,60))

print("가격과 현재시가 비교 5% 에서 필터링")
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


t <- sapply(ll,compare_mv_new,price=c("mv_20","mv_60"),alpha=c(0.05,0.01),"upper")
t2060<-remove_null_list(t)
t2060list<-kosdaq[which(kosdaq$code %in% names(t2060)),2]
print("##20일선의 5% 60일선의 1%")
t2060list



