#current date
#training period - 3~6 months
#sell date - 3~15 days later
#buy recommend condition - min(MSET) & RSI >= 70
#number of stock item - 100

library(MASS)
library(TTR)
source("./lib/mset_Regress.r")
nor <- function(x){(x-min(x))/(max(x)-min(x))}



dat <- readRDS('./dat_price.rds')
colnames(dat)[21] <- 'LG Corp'

#current_date <- '2016-08-01'
current_date <- 91
test_period <- 159
training_period <- 90
#sell_date <- 1
stockItemNum <- NULL
rsiLimit <- 70
recommendItem <- 50

adjustDay <- 0
buyList <- NULL
sellList <- NULL
sellDateList <- NULL
buyListByDay <- rep(0,300)
IncomeListByDay <- rep(0,300)
#current_date <- as.Date(current_date)

ptm <- proc.time()
for( tp in 0:test_period){
  tr_startDate <- current_date-training_period
  tr_endDate <- current_date-1
  #print(paste('거래 일:',current_date,',TR Start:',tr_startDate,',TR End:', tr_endDate))
  tr_dat <- dat[tr_startDate:tr_endDate, ]
  m = mset_Regress(tr_dat,tr_dat)
  
  #항목 추천
  mset <- m$resid_ts
  for(i in 1:ncol(mset)){
    #mset[,i] <- EMA(scale(mset[,i]),n=2)
    mset[,i] <- EMA(5*scale(mset[,i]),n=10)
  }
  
  
  #MSET Filter
  mset_recommend_list <- colnames(sort(mset[nrow(mset),]))[1:min(recommendItem,100)]
  
  #MSET Filter
  for(mset_recommend in mset_recommend_list){
    #RSI Filter
    if( tail(RSI(tr_dat[,mset_recommend]),1) >= rsiLimit){
      #매수
      buyPrice <- dat[current_date,mset_recommend]
      buyList <- c(buyList,buyPrice)
      
      #매도
      sellDate <- NULL
      for(i in 1:10){
        sellDate <- i
        sellPrice <- dat[current_date+i,mset_recommend]
        if(sellPrice >= buyPrice * 1.02){
          break()
        }
      }
      sellList <- c(sellList,sellPrice)
      #sellDateList <- c(sellDateList,sellDate)
      #income <- sellPrice-buyPrice
      #buyListByDay[current_date] <- buyListByDay[current_date] + buyPrice
      #IncomeListByDay[current_date] <- IncomeListByDay[current_date] + income 
      
      #print(paste('      Buy:',mset_recommend,',Price:',buyPrice))
      #print(paste('     Sell:',mset_recommend,',Price:',sellPrice,',Sell Date:',sellDate,',Income:',income))
      
      
    }
  }

  current_date <- current_date + 1
}
proc.time() - ptm



#병렬 처리 - 시작
library(parallel)
# 코어 개수 획득
numCores <- parallel::detectCores() - 1
# 클러스터 초기화
myCluster <- parallel::makeCluster(numCores)

parallel::clusterExport(myCluster, c("dat","mset_Regress","ginv","EMA","RSI"))
# CPU 병렬처리

ptm <- proc.time()
result2 <- parSapply(myCluster, as.character(91:(test_period+91)), 
          function(x){
            buySum <- 0
            sellSum <- 0
            x <- as.numeric(x)
            tr_dat <- dat[(x-90):(x-1), ]
            m = mset_Regress(tr_dat,tr_dat)
            #항목 추천
            mset <- m$resid_ts
            for(i in 1:ncol(mset)){
              mset[,i] <- EMA(5*scale(mset[,i]),n=10)
            }
            #MSET Filter
            mset_recommend_list <- colnames(sort(mset[nrow(mset),]))[1:min(50,100)]
            
            #MSET Filter
            for(mset_recommend in mset_recommend_list){
              #RSI Filter
              if( tail(RSI(tr_dat[,mset_recommend]),1) >= 70){
                #매수
                buyPrice <- dat[x,mset_recommend]
                buySum <- buySum + buyPrice
                
                #매도
                sellPrice <- 0
                for(i in 1:10){
                  sellPrice <- dat[x+i,mset_recommend]
                  if(sellPrice >= buyPrice * 1.02){
                    break()
                  }
                }
                sellSum <- sellSum + sellPrice
              }
            }
            
            c(buyList = buySum, sellList = sellSum)
          })
proc.time() - ptm
# 클러스터 중지
parallel::stopCluster(myCluster)
#병렬 처리 - 끝


#Parallel Model 3

#Step1 - Modeling
