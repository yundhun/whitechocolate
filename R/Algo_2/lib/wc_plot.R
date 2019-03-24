wc_plot_1 <- function(clt_trdat,mset_recommend,mset2){
  clt_trdat_tmp <- clt_trdat[(nrow(clt_trdat)-2):nrow(clt_trdat),]
  first_flg = T
  for(i in 1:ncol(clt_trdat_tmp)){
    if(first_flg){
      plot(nor(clt_trdat_tmp[,i]),type='l',col='gray', main=mset_recommend)
      first_flg = F
    }else{
      points(nor(clt_trdat_tmp[,i]),type='l',col='gray')
    }
  }
  points(nor(clt_trdat_tmp[,mset_recommend]),type='l',col='red')
  points(nor(mset2[,mset_recommend]),type='l',col='blue')
}

wc_plot_2 <- function(clt_trdat,mset_recommend,mset2,today_date){
  clt_trdat_tmp <- clt_trdat[(nrow(clt_trdat)-2):nrow(clt_trdat),]
  min_y <- min(clt_trdat_tmp[,mset_recommend], mset2[,mset_recommend])
  max_y <- max(clt_trdat_tmp[,mset_recommend], mset2[,mset_recommend])
  plot(clt_trdat_tmp[,mset_recommend],type='l',col='red', ylim=c(min_y,max_y), main=paste0(today_date,':',mset_recommend), sub ='red: Actual, blue: Pred')
  points(mset2[,mset_recommend],type='l',col='blue')
}