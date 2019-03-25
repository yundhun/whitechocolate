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
  dat = readRDS('../kosdaq/all_stock_lastprice.rds')
  colnames(dat) <- paste0('S_', colnames(dat))
  idx = which(dat[,1]==target_date)
  tdat = dat[(idx-n_period+1):idx,]
  return(tdat)
}
