
#데이터 로드
dat <- readRDS('dat/dat_price.rds')
dat <- readRDS('dat/dat3.rds')
dat <- readRDS('c:/dev/r/quant/kosdaq_histlist_2018_04_08.rds')
dat <- readRDS('./dat/dat4.rds')

colnames(dat) <- paste0("X",colnames(dat))
dat <- dat[,-nearZeroVar(dat)]

rm_cols = c()
for(x in 1:ncol(dat)){
  if(  any(is.na(dat[,x])) ){
    rm_cols <- c(rm_cols,x)
  }
}
dat <- dat[,-rm_cols]

colnms <- colnames(dat)
colnms <- gsub(" ", "", colnms, fixed = TRUE)
colnms <- gsub("&", "", colnms, fixed = TRUE)
colnms <- gsub("-", "", colnms, fixed = TRUE)
colnames(dat) <- colnms
#분석 시작 날짜
start_date <- 30
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


#Library Load
if(!require(parallel)) {
  install.packages("parallel")
}
library(parallel)
library(MASS)
library(TTR)
library(ClustOfVar)
source("lib/mset_Regress.r")

#Clustering
clust2 <- ClustOfVar::kmeansvar(X.quanti = dat, init=5)
clust2 <- clust2$var
nor <- function(x){ (x-min(x))/(max(x)-min(x)) }



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

result <- matrix(0, nrow = 1, ncol = 6)
#for(predict_period in 1:10){
predict_period <- 3

#Modeling - Start
numCores <- parallel::detectCores() - 1
myCluster <- parallel::makeCluster(numCores)
parallel::clusterExport(myCluster, c("dat","mset_Regress","ginv","EMA","RSI","training_period","clust","start_date",
                                     "predict_period","test_period"))

m <- parSapply(myCluster, as.character(1:length(clust)), 
               function(x){
                 x <- as.numeric(x)
                 if(length(clust[[x]]) < 2) return(NULL)
                 stockNms <- clust[[x]]
                 tdat <- dat[,stockNms]
                 
                 model <- list()
                 for(i in start_date:(start_date+test_period-predict_period)){
                   tdat2 <- tdat[(i-training_period):(i-1), ]
                   md = mset_Regress(tdat2,predict_period)
                   temp <- as.data.frame(md$predict)
                   model[[i]] <- temp
                 }
                 ret <- list(num = model)
               })

parallel::stopCluster(myCluster)
#Modeling - End

#for(msetRange in 1:predict_period){
msetRange <- 2
  #for(rsiLimit in c(52,54,56,58,60,62,64,66,68,70)){
rsiLimit <- 70
print(paste(predict_period,',',msetRange,',',rsiLimit))

#Logic - Start
money <- rep(0,300)
buyList <- NULL
sellList <- NULL
sellDateList <- NULL
for( clustNum in 1:length(clust)){
  
  if(is.null(clust[[clustNum]]) || length(clust[[clustNum]]) < 2) next()
  
  stockNms <- clust[[clustNum]]
  tdat <- dat[,stockNms]
  current_date <- start_date
  
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
          # rsi_dat <- tdat[(tr_startDate+i):(tr_endDate+i), ]
          # rsi_sell <- tail(RSI(rsi_dat[,mset_recommend]),1)
          # rsi_trend <- lm(tail(RSI(rsi_dat[,mset_recommend]),5)~c(1:5))$coefficients[2]
          sellprice_1 <- dat[current_date+i, mset_recommend]
          sellPrice <- stockCnt * dat[current_date+i, mset_recommend]
          if(sellPrice > buyPrice*1.02) {
            money[curDay+i] <- money[curDay+i] + sellPrice
            sellDate = curDay + i
            break()
          }
          #if(rsi_sell > 65 && rsi_trend > 0 ) break()
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
    #current_date <- current_date + 1
  } 
}

win_rate_temp <- length( which( (sellList-buyList)>0 ))/ length(sellList)
str <- paste(sum( sellList-buyList ),
        ' ,', length( which( (sellList-buyList)>0 )),'/',length(sellList) ,
        ' ,',win_rate_temp, sep='')
print(str)

result <- rbind(result, c(predict_period, msetRange, rsiLimit,length(sellList), length( which( (sellList-buyList)>0 )),win_rate_temp ))

#     }
#   }
# }
#Logic - End

plot(dat[,'stock90'], type='l')
abline(v=c(115), col='red')