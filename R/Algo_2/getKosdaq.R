library(qcc)
library(parallel)
library(stringr)
library("XML")
kosdaq_table <- readRDS("./dat/kosdaq.rds")
#kosdaq <- readRDS("./dat/kosdaq.rds")
start_date <- Sys.Date()
rangevalue=10
meanfactor=2.5
alpha=0.05
candidate<-c()
url="http://finance.naver.com/item/sise_day.nhn?code="

histlist <- list()
kosdaq <- kosdaq_table
candidate2<-c()
for(j in 1:nrow(kosdaq)){
  code <- kosdaq$code[j]
  url2=paste0(url,code)
  hist <- data.frame()
  for(i in 1:rangevalue){
    url_make=paste0(url2,"&page=",i)
    ll <- readHTMLTable(url_make, stringsAsFactors = F)
    hist <- rbind(hist,ll[[1]])
  }
  
  hist<-hist[complete.cases(hist[,2]),]
  colnames(hist) <- c("date","last_price","before_price","start_price","high_price","low_price","volume")
  hist$date<-as.Date(hist$date,"%Y.%m.%d")
  hist<-cbind(hist$date,as.data.frame(sapply(hist[,-1],function(x){ as.numeric(str_replace_all(x,pattern = '[//,]',replacement = ""))  }),stringsAsFactors = F))
  colnames(hist) <- c("date","last_price","before_price","start_price","high_price","low_price","volume")
  hist <- list(hist)
  names(hist) <- code
  histlist <<- append(histlist,hist)
  Sys.sleep(0.5)
}
histlist <- readRDS("./kosdaq_histlist_2018_04_08.rds")
saveRDS(histlist,"./kosdaq_histlist_2018_04_08.rds")


