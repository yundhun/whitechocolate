### one day recommand


#?¼?´ë¸ŒëŸ¬ë¦? ë¡œë“œ
source('./lib/load_libs.R')

update_last_price(start_date=Sys.Date()-1)

#Parameter Setting
training_period <- 100 #Training ë²”ìœ„
n_clust <- 5 #Cluster ê°œìˆ˜
#MSET ê´€? ¨
predict_period <- 3 #?˜ˆì¸? ê¸°ê°„
recommendItem <- 50 #MSET ì¶”ì²œ ê°??ˆ˜
msetRange <- 2
#RSI ê´€? ¨
rsi_limit <- 60 #RSI ê¸°ì?€
rsi_trend_days <- 5 #RSI ê²½í–¥ ë¶„ì„ ê¸°ê°„


today_date <- Sys.Date()-1

trdat = get_train_data_new(today_date, training_period)

trdat = trdat[,-1] #?‚ ì§? ? œê±?

#Step2-2 : Clustering
clust = clust_stocks(trdat,n_clust)
for( clt in clust){
  #Select Cluster data from Train Data
  clt_trdat = trdat[,as.vector(clt)]
  #MSET
  mset = mset_Regress_RF(clt_trdat,predict_period)$predict
  mset2 = mset_Regress_RF(clt_trdat,predict_period)$predict2
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
      #wc_plot_2(clt_trdat,mset_recommend,mset2, today_date)
      #bid(bid_dat)
    }
  }    
  
}

sendEmail(bid_dat)
