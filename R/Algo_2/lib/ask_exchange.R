ask_by_rule <- function(ask_targets, ask_rule){
  if( (sellPricePerOne > 1.05*buyPricePerOne) || (today_date - buyDate >= 10) || (sellPricePerOne < 0.95*buyPricePerOne)){
    transaction_history[transaction_history[,'uuid']==uuid,'status'] <- '2'
    myMoney <- myMoney+totalSellPrice
    transaction_history <- rbind(transaction_history,stock_transaction)
  }