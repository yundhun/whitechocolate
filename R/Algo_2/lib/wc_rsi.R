#RSI Filter
#rsi_rule = list('rsi_limit':rsi_limit,'rsi_trend_days':5)
rsi_filter <- function(dat, mset_recommend, rsi_rule){
  #print(paste('RSI:',tail(RSI(dat[,mset_recommend]),1)))
  #print(lm(tail(RSI(dat[,mset_recommend]), rsi_rule$'rsi_trend_days')~c(1:rsi_rule$'rsi_trend_days'))$coefficients[2])
        
  if(tail(RSI(dat[,mset_recommend]),1) >= rsi_rule$'rsi_limit' &&
      lm(tail(RSI(dat[,mset_recommend]), rsi_rule$'rsi_trend_days')~c(1:rsi_rule$'rsi_trend_days'))$coefficients[2] > 0 ){ 
    return(TRUE)
  }
  return(FALSE)
}
