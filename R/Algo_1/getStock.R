library(lubridate)
library(httr)
library(rvest)
library(xts)
library(tibble)
library(dplyr)

kosdaq_table <- readRDS("./dat/kosdaq.rds")
#kosdaq <- readRDS("./dat/kosdaq.rds")
start_date <- Sys.Date()
rangevalue=500
histlist <- list()
kosdaq <- kosdaq_table
for(j in 1:nrow(kosdaq)){
  code <- kosdaq$code[j]
  url2 <- paste0("https://fchart.stock.naver.com/sise.nhn?symbol="
                 ,code,"&timeframe=day&count=",rangevalue,"&requestType=0")
  hist <- GET(url2) %>%
    read_html(encoding = "euc-kr") %>%
    html_nodes("item") %>%
    html_attr("data") %>%
    strsplit("\\|") %>% rev
  
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

saveRDS(histlist,file="../kosdaq/kosdaq_1000_20190324.rds")

head(histlist)


###°¡°Ýrds
hist <- lapply(histlist, function(x) {
  x$last_price %>% data.frame(stringsAsFactors = F)
})
hist <- do.call(cbind, hist)
colnames(hist) <-names(histlist)
hist<-cbind(data=histlist[[1]]$date,hist)
saveRDS(hist,"../kosdaq/kosdaq_500_20190324_lastprice.rds")





