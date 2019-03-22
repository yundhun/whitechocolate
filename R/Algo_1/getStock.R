library(lubridate)
library(httr)
library(rvest)
library(xts)
library(tibble)

kosdaq_table <- readRDS("./dat/kosdaq.rds")
#kosdaq <- readRDS("./dat/kosdaq.rds")
start_date <- Sys.Date()
rangevalue=100
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
  hist <- add_column(hist, c(diff(as.numeric(hist[,2])),0), .after = 2)
  colnames(hist) <- c("date","last_price","before_price","start_price","high_price","low_price","volume")
  hist <- list(hist)
  names(hist) <- code
  histlist <<- append(histlist,hist)
  Sys.sleep(0.5)
}