#라이브러리 로드
source('./lib/wc_libs.R')
source("lib/mset_Regress.r")
library(parallel)
library(MASS)
library(TTR)
library(ClustOfVar)
library(caret)

#데이터 로드
full_dat <- readRDS('./dat/dat4.rds')
full_dat <- dat_prep_1(full_dat)

#오늘 날짜
today_date <- 30
#분석 기간
test_period <- 60
#Training 범위
training_period <- 30
#예측 기간
predict_period <- 3
#RSI 기준
rsiLimit <- 70
#MSET 추천 갯수
recommendItem <- 50
msetRange <- 2

#활용 데이터
dat <- full_dat[(today_date-training_period+1):today_date,]
dat <- dat_prep_1(dat)

#Step 1 : Clustering
clust = clust_stocks(dat,5)

#Step 2 
for( clustNum in 1:length(clust)){
  if(is.null(clust[[clustNum]]) || length(clust[[clustNum]]) < 2) next()
  stockNms <- clust[[clustNum]]
  tdat <- dat[,stockNms]
  
  for( stockNm in stockNms){
    m <- mset_stock(tdat,stockNm,predict_period)
    
    te_dat <- tdat[(nrow(tdat)-predict_period+1):nrow(tdat),]
    pred_dat <- predict(m, newdata = te_dat)
    
    #MSET Filter
    mset <- m1$num[[curDay]]
    if(is.na(all(mset))) next()
    if( round(ncol(mset)*0.8) < 2) next()
    mset2 <- sort(colSums(abs(mset[1:3,])))[1:max(1, round(ncol(mset)*0.8) )]
    mset <- mset[, names(mset2)]
    colnames(mset)
    mset_recommend_list <- colnames(mset[,colMeans(tail(mset,msetRange)) < 0])    
  }
  
  m1 <- m[[clustNum]]
  
  for( curDay in current_date:(current_date+test_period-predict_period)){
    tr_startDate <- curDay-training_period
    tr_endDate <- curDay-1
    tr_dat <- tdat[tr_startDate:tr_endDate, ]
    
    print(paste('Cluster',clustNum,',거래 일:',curDay,',분석 범위(날짜):',tr_startDate,'~', tr_endDate))
    
    #MSET Filter
    mset <- m1$num[[curDay]]
    if(is.na(all(mset))) next()
    if( round(ncol(mset)*0.8) < 2) next()
    mset2 <- sort(colSums(abs(mset[1:3,])))[1:max(1, round(ncol(mset)*0.8) )]
    mset <- mset[, names(mset2)]
    colnames(mset)
    mset_recommend_list <- colnames(mset[,colMeans(tail(mset,msetRange)) < 0])
    
    for(mset_recommend in mset_recommend_list){
      #RSI Filter
      if( tail(RSI(tr_dat[,mset_recommend]),1) >= rsiLimit &&
          lm(tail(RSI(tr_dat[,mset_recommend]),5)~c(1:5))$coefficients[2] > 0
      ){
        #매수
        stockPrice <- dat[current_date,mset_recommend]
        stockCnt <- max(1, round(1000000/stockPrice))
        buyPrice <- stockCnt * stockPrice
        buyList <- c(buyList,buyPrice)
        
        money[curDay] <- money[curDay] - buyPrice
        
        #매도
        sellPrice <- 0
        sellDate <- NULL
        sellprice_1 <- 0
        for(i in 1:20){
          sellprice_1 <- dat[current_date+i, mset_recommend]
          sellPrice <- stockCnt * dat[current_date+i, mset_recommend]
          if(sellPrice > buyPrice*1.02) {
            money[curDay+i] <- money[curDay+i] + sellPrice
            sellDate = curDay + i
            break()
          }
        }
        
        sellList <- c(sellList,sellPrice)
        income <- sellPrice-buyPrice
        print(paste('      Buy:',mset_recommend,',Price:',buyPrice,',1:',stockPrice))
        print(paste('     Sell:',mset_recommend,',Price:',sellPrice,',Sell Date:',sellDate,',Income:',income,',1:',sellprice_1))
        plot(dat[,mset_recommend], type='l')
        abline(v=curDay, col='blue')
        abline(v=sellDate, col='red')
      }
    }
    
    money[curDay+1] <- money[curDay+1] + money[curDay]
  } 
}
str <- paste(sum( sellList-buyList ),
             ' ,', length( which( (sellList-buyList)>0 )),'/',length(sellList) ,
             ' ,',win_rate_temp, sep='')
print(str)