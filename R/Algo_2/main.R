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

myMoney <- 10000000
transaction_history <- NULL

#활용 데이터
dat <- full_dat[(today_date-training_period+1):today_date,]
dat <- dat_prep_1(dat)
#trdat <- dat[1:(nrow(dat)-1),]
tr_dat <- dat
#Step 1 : Clustering
clust = clust_stocks(dat,5)

#Step 2 
for( clustNum in 1:length(clust)){
  if(is.null(clust[[clustNum]]) || length(clust[[clustNum]]) < 2) next()
  stockNms <- clust[[clustNum]]
  tdat <- dat[,stockNms]

  #Mset
  mset <- mset_Regress(tdat,predict_period)$predict
  
  #Mset Filter
  mset_recommend_list <- mset_filter(mset,predict_period,msetRange)
  
  for(mset_recommend in mset_recommend_list){
    #RSI Filter
    if( tail(RSI(tr_dat[,mset_recommend]),1) >= rsiLimit &&
        lm(tail(RSI(tr_dat[,mset_recommend]),5)~c(1:5))$coefficients[2] > 0
    ){
      #매수
      buyPricePerOne <- dat[nrow(dat),mset_recommend]
      stockCnt <- max(1, round(100000/buyPricePerOne))
      totalBuyPrice <- stockCnt * buyPricePerOne
      myMoney <- myMoney-totalBuyPrice
      print(paste('거래일',today_date,':',mset_recommend,'종목 매수. 갯수:',stockCnt,'개당 매수 가격:',buyPricePerOne,'총 매수가격:',totalBuyPrice))
      #curDate,myMoney,stockNm,buyDate,buyPricePerOne,buyNumbers,totalBuyPrice,sellDate,sellPricePerOne,sellNumbers,totalSellPrice,Income,status
      stock_transaction <- c(today_date,
                             myMoney,
                             mset_recommend,
                             today_date,
                             buyPricePerOne,
                             stockCnt,
                             totalBuyPrice,
                             '',0,0,0,0,1
                             )
      transaction_history <- rbind(transaction_history,stock_transaction)
      #curDate,myMoney,stockNm,buyDate,buyPricePerOne,buyNumbers,totalBuyPrice,sellDate,sellPricePerOne,sellNumbers,totalSellPrice,Income,status
      #status 1:on hand 2:sold
      }
  }
  
}
colnames(transaction_history) <- c('curDate','myMoney','stockNm','buyDate','buyPricePerOne','buyNumbers','totalBuyPrice','sellDate','sellPricePerOne','sellNumbers','totalSellPrice','income','status')
