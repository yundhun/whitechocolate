library(lubridate)
library(httr)
library(rvest)
library(xts)
library(tibble)
library(dplyr)

get_stock<-function(code,rangevalue,sleep=0.5,start_date=Sys.Date()){
  url2 <- paste0("https://fchart.stock.naver.com/sise.nhn?symbol="
                 ,code,"&timeframe=day&count=",rangevalue,"&requestType=0")
  hist <- GET(url2) %>%
    read_html(encoding = "euc-kr") %>%
    html_nodes("item") %>%
    html_attr("data") %>%
    strsplit("\\|") %>% rev
  
##  if(length(hist)< rangevalue ){
##    return (NULL)
##  }
##  if(start_date != as.Date(hist[[1]][1],"%Y%m%d")){
##    return (NULL)
##  }
  
  
  hist <- lapply(hist, function(x) {
    x %>% t() %>% data.frame(stringsAsFactors = F)
  })
  hist <- do.call(rbind, hist)
  if(nrow(hist)>1){
    hist <- cbind(as.Date(hist[,1],"%Y%m%d"),as.data.frame(sapply(hist[,-1],function(x){ as.numeric(x)})))
  }else{
    hist[,1]<-as.Date(hist[1,1],"%Y%m%d")
    hist[,-1]<-as.numeric(hist[,-1])
  }
  hist <- add_column(hist, c(diff(as.numeric(hist[,5])),0), .after = 2)
  colnames(hist) <- c("date","start_price","before_price","high_price","low_price","last_price","volume")
  hist <- list(hist)
  names(hist) <- code
  Sys.sleep(sleep)
  return (hist)  
}

update_last_price <- function(start_date=Sys.Date(),file_path="../kosdaq/",stock_table="all_stock_table.rds",file_name="all_stock_lastprice.rds",sleep=0.5){
  
  mset_stock_train_dat <-readRDS(paste0(file_path,file_name))
  code_table <-readRDS(paste0(file_path,stock_table))
  base_date <- mset_stock_train_dat[nrow(mset_stock_train_dat),1]
  rangevalue <- as.numeric(start_date-base_date)
  
  
  if(rangevalue<1){
    stop("alread update")
  }
  codenames <- colnames(mset_stock_train_dat)
  insert_index <- NULL
  for(i in 2:length(codenames)){
  
    code <- codenames[i]
    stock <- get_stock(code,rangevalue,sleep=sleep,start_date=start_date)
    
    if(is.null(stock)){
      next
    }
    
    
    if(code!=names(stock)){
      stop("code is not eqaul get_stock")
    }

    stock <- stock[[1]]
    
    
    stock <- stock[which(stock$date>base_date),]
   
    if(nrow(stock)<1){
      stop("alread update")
    }
    stock <- stock[nrow(stock):1,]
    if(is.null(insert_index)){
     m<-data.frame(matrix(nrow=nrow(stock),ncol=ncol(mset_stock_train_dat)))  
     colnames(m)<-colnames(mset_stock_train_dat)
     m[,1] <- as.Date(stock$date,"%Y%m%d")
     
     insert_index <- seq(nrow(mset_stock_train_dat)+1,nrow(mset_stock_train_dat)+nrow(stock),by=1) 
     mset_stock_train_dat<-rbind(mset_stock_train_dat,m)  
    }
   
  
    if(length(insert_index)>nrow(stock)){
      mset_stock_train_dat[which(mset_stock_train_dat$date %in% stock$date),i] <- stock$last_price
    }
    else {mset_stock_train_dat[insert_index,i] <- stock$last_price
    }
  }
  rmname <- which(colnames(mset_stock_train_dat) %in% colnames(mset_stock_train_dat)[colSums(is.na(mset_stock_train_dat)) > 0])
  mset_stock_train_dat <- mset_stock_train_dat[,-rmname]
  saveRDS(mset_stock_train_dat,paste0(file_path,file_name))
  colnames(mset_stock_train_dat) <- paste0('S_', colnames(mset_stock_train_dat))
  assign("mset_stock_train_dat", mset_stock_train_dat, envir = .GlobalEnv)
}
