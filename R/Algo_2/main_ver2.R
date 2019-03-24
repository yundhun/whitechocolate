#라이브러리 로드
source('./lib/load_libs.R')

#Parameter Setting
training_period <- 100 #Training 범위
n_clust <- 5 #Cluster 개수
#MSET 관련
predict_period <- 3 #예측 기간
recommendItem <- 50 #MSET 추천 갯수
msetRange <- 2
#RSI 관련
rsi_limit <- 60 #RSI 기준
rsi_trend_days <- 5 #RSI 경향 분석 기간

testDateList <- readRDS('./testDateList.rds')

for ( today_date in testDateList){
  print(paste('#Date: ',today_date))

  #----------
  #Step1 매도
  #----------
  #Step1-1 : 매도 대상 조회([상태 코드] 0:매수완료(매도 안함) 1:매수 후 매도 완료)
  #ask_targets = get_tr_by_status(0)
  
  #Step1-2 : 매도 조건에 따라 매도
  #ask_rule = list('loss_cut'=0.05, 'profit_cut'=0.05, 'loss_cut_days'=20)
  #ask_by_rule(ask_targets, ask_rule)

  #----------
  #Step2 매수
  #----------
  #Step2-1 : Get Train Data
  trdat = get_train_data(today_date, training_period)
  trdat = trdat[,-1] #날짜 제거
  
  #Step2-2 : Clustering
  clust = clust_stocks(trdat,n_clust)
  
  #Step2-3 : Cluster 별로 매수 대상 분석
  for( clt in clust){
    #Select Cluster data from Train Data
    clt_trdat = trdat[,as.vector(clt)]
    #MSET
    mset = mset_Regress(clt_trdat,predict_period)$predict
    mset2 = mset_Regress(clt_trdat,predict_period)$predict2
    mset_recommend_list = mset_filter(mset,predict_period,msetRange)
    for(mset_recommend in mset_recommend_list){
      rsi_rule = list('rsi_limit'=rsi_limit,'rsi_trend_days'=rsi_trend_days)
      result = rsi_filter(clt_trdat, mset_recommend, rsi_rule)
      if(result){
        price = tail(clt_trdat[,mset_recommend],1)
        volume = max(1, round(500000/price))
        bid_dat = list('id'=UUIDgenerate(),
                       'tr_datetime'=today_date,
                       'stock_name'=mset_recommend,
                       'volume'=volume,
                       'price'=price)
        print(bid_dat)
        #bid_insert_db(bid_dat)
        #sendEmail(bid_dat)
        wc_plot_2(clt_trdat,mset_recommend,mset2, today_date)
        #bid(bid_dat)
      }
    }    
    
  }
}
