library(qcc)
library(parallel)
library(stringr)
library("XML")
kosdaq_table <- readRDS("./dat/kosdaq.rds")
#kosdaq <- readRDS("./dat/kosdaq.rds")
start_date <- Sys.Date()
rangevalue=2
meanfactor=2.5
alpha=0.05
candidate<-c()
url="http://finance.naver.com/item/sise_day.nhn?code="

numCores <- parallel::detectCores() - 1
myCluster <- parallel::makeCluster(numCores)
index <- split(1:nrow(kosdaq_table), ceiling(seq_along(1:nrow(kosdaq_table))/50))
clusterExport(myCluster, c("rangevalue","meanfactor","alpha","candidate","url","kosdaq_table","index"))


m<-parallel::parLapply(cl = myCluster, X = index[1:3], fun = function(x) {
  library(stringr)
  library("XML")
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
    
    basevolume <- mean(hist$volume)*meanfactor
    
    isthere<-which(hist$volume>=basevolume)

    if (length(isthere)>0){
      basep<-NULL
      maxp<-hist[which.max(hist$volume),]
      
      islarge <- ((maxp$start_price+maxp$high_price)/2*maxp$volume>5000000000)
      isup <- (maxp$last_price-maxp$start_price>0) 
      
      if(islarge&isup){
        testp = maxp
        while(testp$volume>basevolume){
          rowname <- as.numeric(row.names(testp))
          if((rowname+1)<=nrow(hist)) {
            testp <- hist[(rowname+1),]
          }
          else {
            break
          }
        }
        
        rowname <- as.numeric(row.names(testp))
        if ((rowname-1)>0) basep <- hist[(rowname-1),]
        else basep <-testp
        
        judgea<-abs((basep$start_price-hist[1,]$last_price))/basep$start_price
        if(judgea<=alpha){
          candidate2<<-c(candidate2,code)
        }
      }
    }
   
  }
  candidate2
})

parallel::stopCluster(myCluster)






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
  
  basevolume <- mean(hist$volume)*meanfactor
  
  isthere<-which(hist$volume>=basevolume)
  
  if (length(isthere)>0){
    basep<-NULL
    maxp<-hist[which.max(hist$volume),]
    
    islarge <- ((maxp$start_price+maxp$high_price)/2*maxp$volume>5000000000)
    isup <- (maxp$last_price-maxp$start_price>0) 
    
    if(islarge&isup){
      testp = maxp
      while(testp$volume>basevolume){
        rowname <- as.numeric(row.names(testp))
        if((rowname+1)<=nrow(hist)) {
          testp <- hist[(rowname+1),]
        }
        else {
          break
        }
      }
      
      rowname <- as.numeric(row.names(testp))
      if ((rowname-1)>0) basep <- hist[(rowname-1),]
      else basep <-testp
      
      judgea<-abs((basep$start_price-hist[1,]$last_price))/basep$start_price
      if(judgea<=alpha){
        candidate2<<-c(candidate2,code)
      }
    }
  }
  
}
