library(XML)
library(stringr)
volumntop <- htmlTreeParse("http://finance.naver.com/sise/sise_quant.nhn?sosok=1",useInternal=TRUE,trim=TRUE)
volumnlow <- htmlTreeParse("http://finance.naver.com/sise/sise_quant_low.nhn?sosok=1",useInternal=TRUE,trim=TRUE)


volt<-readHTMLTable(volumntop , which=2, stringsAsFactors = F, header=T)
volt<-volt[complete.cases(volt[,2]),]
rownames(volt) <- volt$N
volt <- volt[,-1]
volt <- volt[,c(1,2,4,5,6,9,10,11)]
colnames(volt) <- c("name","current_price","changesize","volume","volume_price","total_market_price","per","roe")
volt$current_price <- as.numeric(str_replace_all(volt$current_price,pattern = '[//,]',replacement = ""))
volt$changesize <- as.numeric(str_replace_all(volt$changesize,pattern = '[//%]',replacement = ""))
volt$volume <- as.numeric(str_replace_all(volt$volume,pattern = '[//,]',replacement = ""))
volt$volume_price <- as.numeric(str_replace_all(volt$volume_price,pattern = '[//,]',replacement = ""))
volt$total_market_price <- as.numeric(str_replace_all(volt$total_market_price,pattern = '[//,]',replacement = ""))
volt$per <- as.numeric(volt$per)
volt$roe <- as.numeric(str_replace_all(volt$roe ,pattern = '[//,]',replacement = ""))



##
volt<-volt[rev(order(volt$volume_price)),]



#



voll<-readHTMLTable(volumnlow , which=2, stringsAsFactors = F, header=T)
voll<-voll[complete.cases(voll[,2]),]
rownames(voll) <- voll$N
voll <- voll[,-1]
voll <- voll[,c(1,2,3,5,6,9,10,11)]
colnames(voll) <- c("name","current_price","changesize","volume","volume_price","total_market_price","per","roe")
voll$current_price <- as.numeric(str_replace_all(voll$current_price,pattern = '[//,]',replacement = ""))
voll$changesize <- as.numeric(str_replace_all(voll$changesize,pattern = '[//%]',replacement = ""))
voll$volume <- as.numeric(str_replace_all(voll$volume,pattern = '[//,]',replacement = ""))
voll$volume_price <- as.numeric(str_replace_all(voll$volume_price,pattern = '[//,]',replacement = ""))
voll$total_market_price <- as.numeric(str_replace_all(voll$total_market_price,pattern = '[//,]',replacement = ""))
voll$per <- as.numeric(voll$per)
voll$roe <- as.numeric(str_replace_all(voll$roe ,pattern = '[//,]',replacement = ""))





colnames()

grep('[[:punct:]]',volt$current_price)
str_replace_all(volt$volume,pattern = '[//,]',replacement = "")
sub('[[:punct:]]','',volt$volume)
str_replace("1.33","[,]","")
doc.text <- unlist(xpathApply(doc.html, '//p', htmlValue))

data <- xpathSApply(t, "//td[@class = 'title']//[[@class = 'number']", xmlValue, trim = TRUE)

t <- htmlParse("http://finance.naver.com/sise/sise_quant.nhn?sosok=1",useInternal=TRUE,trim=TRUE)
txt <- htmlToText(t)
minutes <- xpathSApply(t,"\\tr",xmlValue)

pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"
plain.text <- gsub(pattern, "\\1", t)

library(RCurl)
library(XML)

# download html
html <- getURL("http://finance.naver.com/sise/sise_quant.nhn?sosok=1", followlocation = TRUE)

# parse html
doc = htmlParse(html, asText=TRUE)
plain.text <- xpathSApply(doc, "//p", xmlValue)
cat(paste(plain.text, collapse = "\n"))