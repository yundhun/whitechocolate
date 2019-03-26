library(lubridate)
library(httr)
library(rvest)
library(xts)
library(tibble)
library(dplyr)
library(DataCombine)

kosdaq_table <- readRDS("./dat/kosdaq.rds")
kospi_table <- readRDS("./dat/kospi.rds")
#kosdaq <- readRDS("./dat/kosdaq.rds")
start_date <- Sys.Date()
rangevalue=500
histlist <- list()
all_table <- rbind(kosdaq_table,kospi_table)

for(j in 1:nrow(all_table)){
  code <- all_table$code[j]
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
  #colnames(hist) <- c("date","last_price","before_price","start_price","high_price","low_price","volume")
  colnames(hist) <- c("date","start_price","before_price","high_price","low_price","last_price","volume")
  
  hist <- list(hist)
  names(hist) <- code
  histlist <<- append(histlist,hist)
  Sys.sleep(0.5)
}

saveRDS(histlist,file="../kosdaq/all_stock.rds")

head(histlist)


###????rds
hist <- lapply(histlist, function(x) {
  x$last_price %>% data.frame(stringsAsFactors = F)
})
hist <- do.call(cbind, hist)
colnames(hist) <-names(histlist)
hist<-cbind(date=histlist[[1]]$date,hist)
hist <- hist[nrow(hist):1,]
row.names(hist) <- 1:nrow(hist)
saveRDS(hist,"../kosdaq/all_stock_lastprice.rds")




