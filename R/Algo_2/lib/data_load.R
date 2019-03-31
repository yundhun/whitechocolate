#Training Data Load
get_train_data <- function(target_date, n_period){
  if(!exists("mset_stock_train_dat")){
    mset_stock_train_dat = readRDS('../kosdaq/all_stock_lastprice.rds')
    colnames(mset_stock_train_dat) <- paste0('S_', colnames(mset_stock_train_dat))
    assign("mset_stock_train_dat", mset_stock_train_dat, envir = .GlobalEnv)
  }
  idx = which(mset_stock_train_dat[,1]==target_date)
  tdat = mset_stock_train_dat[(idx-n_period+1):idx,]
  return(tdat)
}
