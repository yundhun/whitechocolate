library(XML)
library(RCurl)

url <- "http://bigdata-trader.com/itemcodehelp.jsp"
appData <- getURL(url, ssl.verifypeer = FALSE)
doc <- htmlParse(appData)
perftable <- readHTMLTable(doc, stringsAsFactors = F)

perftable<-perftable[[1]]
perftable<-rbind(perftable,colnames(perftable))

colnames(perftable) <- c("code","name","category")
k<-apply(perftable,1,function(x){ paste0(x[3],":",x[1]) })
perftable<- cbind(perftable,k)

KOSPI <- perftable[which(perftable$category=="KOSPI"),]
KOSDAQ <- perftable[which(perftable$category=="KOSDAQ"),]


apply(array, margin, ...)
saveRDS(perftable,"./perftable.rds")
saveRDS(KOSPI,"./kospi.rds")
saveRDS(KOSDAQ,"./kosdaq.rds")