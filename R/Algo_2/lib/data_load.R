#Training Data Load
get_train_data <- function(target_date, n_period){
  dat = readRDS('../kosdaq/kosdaq_500_20190324_lastprice.rds')
  colnames(dat) <- paste0('S_', colnames(dat))
  idx = which(dat[,1]==target_date)
  tdat = dat[idx:(idx+n_period-1),]
  tdat = tdat[nrow(tdat):1,]
  return(tdat)
}

#colnames(dat) <- paste0('S_', colnames(dat))
#str(dat)
get_train_data_new <- function(target_date, n_period){
  if(!exists("mset_stock_train_dat")){
    mset_stock_train_dat = readRDS('../kosdaq/all_stock_lastprice.rds')
    colnames(mset_stock_train_dat) <- paste0('S_', colnames(mset_stock_train_dat))
    assign("mset_stock_train_dat", mset_stock_train_dat, envir = .GlobalEnv)
  }
  idx = which(mset_stock_train_dat[,1]==target_date)
  tdat = mset_stock_train_dat[(idx-n_period+1):idx,]
  return(tdat)
}
