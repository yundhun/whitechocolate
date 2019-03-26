library(lubridate)
library(httr)
library(rvest)
library(xts)
library(tibble)
library(dplyr)

get_stock<-function(code,rangevalue,sleep=0.5){
  url2 <- paste0("https://fchart.stock.naver.com/sise.nhn?symbol="
                 ,code,"&timeframe=day&count=",rangevalue,"&requestType=0")
  hist <- GET(url2) %>%
    read_html(encoding = "euc-kr") %>%
    html_nodes("item") %>%
    html_attr("data") %>%
    strsplit("\\|") %>% rev
  if(length(hist)<rangevalue ){
    next
  }
  hist <- lapply(hist, function(x) {
    x %>% t() %>% data.frame(stringsAsFactors = F)
  })
  hist <- do.call(rbind, hist)
  hist <- cbind(as.Date(hist[,1],"%Y%m%d"),as.data.frame(sapply(hist[,-1],function(x){ as.numeric(x)})))
  hist <- add_column(hist, c(diff(as.numeric(hist[,5])),0), .after = 2)
  colnames(hist) <- c("date","start_price","before_price","high_price","low_price","last_price","volume")
  hist <- list(hist)
  names(hist) <- code
  Sys.sleep(sleep)
  return (hist)  
}

update_last_price <- function(file_path="../kosdaq/",stock_table="all_stock.rds",file_name="kosdaq_500_20190324_lastprice.rds"){
  
  stock_data <-readRDS(paste0(file_path,file_name))
  code_table <-readRDS(paste0(file_path,stock_table))
  start_date <- Sys.Date()
  rangevalue <- as.numeric(start_date-stock_data[1,1])
  base_date <- stock_data[1,1]
  
  if(rangevalue<1){
    stop("alread update")
  }
  codenames <- colnames(stock_data[,-1])
  insert_index <- NULL
  for(i in 1:nrow(code_table)){
  
    code <- code_table$code[i]
    stock <- get_stock(code,rangevalue)
    if(code!=names(stock)){
      stop("code is not eqaul get_stock")
    }
    stock <- stock[[1]]
    stock <- stock[which(stock$date>base_date),]
    if(i==1){
     m<-data.frame(matrix(nrow=nrow(stock),ncol=ncol(stock_data)))  
     colnames(m)<-colnames(stock_data)
     m[,1] <- as.Date(stock$date,"%Y%m%d")
     
     insert_index <- seq(nrow(stock_data)+1,nrow(stock_data)+nrow(stock),by=1) 
     stock_data<-rbind(stock_data,m)  
    }
    if(code!=codenames[i]){
      stop("code is not equal stock_data")
    }
    stock_data[insert_index,i] <- stock$last_price
    
  }
  saveRDS(stock_data,stock_data)
  
}
