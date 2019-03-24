
kosdaq_table <- read.csv("KOSDAQ.txt",stringsAsFactors = F,sep = "\t",encoding = "utf-8")
kosdaq_table[,1]<-trimws(kosdaq_table[,1])
kosdaq_table[,2]<-as.character(trimws(kosdaq_table[,2]))

kospi_table <- read.csv("KOSPI.txt",stringsAsFactors = F,sep = "\t",encoding = "utf-8")
kospi_table[,1]<-trimws(kospi_table[,1])
kospi_table[,2]<-as.character(trimws(kospi_table[,2]))

saveRDS(kosdaq_table,"./dat/kosdaq.rds")
saveRDS(kospi_table,"./dat/kospi.rds")