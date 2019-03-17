#라이브러리 로드
source('./lib/wc_libs.R')
source("lib/mset_Regress.r")
library(parallel)
library(MASS)
library(TTR)
library(ClustOfVar)
library(caret)
library(uuid)

#데이터 로드
full_dat <- readRDS('./dat/dat4.rds')
full_dat <- dat_prep_1(full_dat)

#Training 범위
training_period <- 40
#예측 기간
predict_period <- 3
#RSI 기준
rsiLimit <- 70
#MSET 추천 갯수
recommendItem <- 50
msetRange <- 2

myMoney <- 10000000
transaction_history <- NULL
#today_date ; 오늘 날짜 
for ( today_date in training_period:100){
  #
  print(paste('#Date: ',today_date))
  
  #활용 데이터
  dat <- full_dat[(today_date-training_period+1):today_date,]
  dat <- dat_prep_1(dat)
  #Step 1 : Clustering
  clust = clust_stocks(dat,5)
  
  #Step 2 : 매도
  print('매도 진행중')
  if(!is.null(transaction_history) && !is.null(nrow(transaction_history[transaction_history[,'status']==1,]))){
    target_transactions <- transaction_history[transaction_history[,'status']==1,]
    for( i in 1:nrow(target_transactions)){
      target_transaction <- target_transactions[i,]
      uuid <- as.character(target_transaction['uuid'])
      stockNm <- as.character(target_transaction['stockNm'])
      buyPricePerOne <- as.integer(as.character(target_transaction['buyPricePerOne']))
      buyDate <- as.integer(as.character(target_transaction['buyDate']))
      buyNumbers <- as.integer(as.character(target_transaction['buyNumbers']))
      totalBuyPrice <- as.integer(as.character(target_transaction['totalBuyPrice']))
      sellPricePerOne <- dat[nrow(dat),stockNm]
      totalSellPrice <- sellPricePerOne*buyNumbers
      income <- totalSellPrice-totalBuyPrice
      status <- 2

      #curDate,myMoney,stockNm,buyDate,buyPricePerOne,buyNumbers,totalBuyPrice
      #sellDate,sellPricePerOne,sellNumbers,totalSellPrice,Income,status
      #status 1:on hand 2:sold      
      if( (sellPricePerOne > 1.05*buyPricePerOne) || (today_date - buyDate >= 10) || (sellPricePerOne < 0.95*buyPricePerOne)){
        transaction_history[transaction_history[,'uuid']==uuid,'status'] <- '2'
        myMoney <- myMoney+totalSellPrice
        stock_transaction <- c(today_date,
                               myMoney,
                               stockNm,
                               buyDate,
                               buyPricePerOne,
                               buyNumbers,
                               totalBuyPrice,
                               today_date,
                               sellPricePerOne,
                               buyNumbers,
                               totalSellPrice,
                               income,2,UUIDgenerate())
        transaction_history <- rbind(transaction_history,stock_transaction)
      }
    }
  }
   
  
  #Step 3 : 매수
  print('매수 진행중')
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
      if( tail(RSI(dat[,mset_recommend]),1) >= rsiLimit &&
          lm(tail(RSI(dat[,mset_recommend]),5)~c(1:5))$coefficients[2] > 0
      ){
        #매수
        buyPricePerOne <- dat[nrow(dat),mset_recommend]
        stockCnt <- max(1, round(500000/buyPricePerOne))
        totalBuyPrice <- stockCnt * buyPricePerOne
        myMoney <- myMoney-totalBuyPrice
        if(myMoney < 0) next()
        #print(paste('거래일',today_date,':',mset_recommend,'종목 매수. 갯수:',stockCnt,'개당 매수 가격:',buyPricePerOne,'총 매수가격:',totalBuyPrice))
        #curDate,myMoney,stockNm,buyDate,buyPricePerOne,buyNumbers,totalBuyPrice,sellDate,sellPricePerOne,sellNumbers,totalSellPrice,Income,status,uuid
        stock_transaction <- c(today_date,
                               myMoney,
                               mset_recommend,
                               today_date,
                               buyPricePerOne,
                               stockCnt,
                               totalBuyPrice,
                               '',0,0,0,0,1,UUIDgenerate()
                               )
        transaction_history <- rbind(transaction_history,stock_transaction)
        #curDate,myMoney,stockNm,buyDate,buyPricePerOne,buyNumbers,totalBuyPrice,sellDate,sellPricePerOne,sellNumbers,totalSellPrice,Income,status,uuid
        #status 1:on hand 2:sold
        }
    }
    if(!is.null(transaction_history)){
      #transaction_history <- as.data.frame(transaction_history)
      colnames(transaction_history) <- c('curDate','myMoney','stockNm','buyDate','buyPricePerOne','buyNumbers','totalBuyPrice','sellDate','sellPricePerOne','sellNumbers','totalSellPrice','income','status','uuid')
      transaction_history[,'stockNm'] <- as.character(transaction_history[,'stockNm'])
      transaction_history[,'uuid'] <- as.character(transaction_history[,'uuid'])
    }
    
  }
}

my_cash <- as.integer(transaction_history[nrow(transaction_history),'myMoney'])
my_asset_value <- 0
my_assets <- transaction_history[transaction_history[,'status']==1,]

if(is.null(nrow(my_assets))){
  my_asset <- my_assets
  cur_val <- as.integer(dat[nrow(dat),my_asset['stockNm']])
  cnt <- as.integer(my_asset['buyNumbers'])
  my_asset_value <- my_asset_value + (cur_val * cnt)
}else{
  for(i in 1:nrow(my_assets)){
    my_asset <- my_assets[i,]
    cur_val <- as.integer(dat[nrow(dat),my_asset['stockNm']])
    cnt <- as.integer(my_asset['buyNumbers'])
    my_asset_value <- my_asset_value + (cur_val * cnt)
  }
}

print(paste('My Cash:',my_cash))
print(paste('My Asset:',my_asset_value))
print(paste('Total:',(my_cash+my_asset_value)))

sum(as.integer(transaction_history[,'income']))
