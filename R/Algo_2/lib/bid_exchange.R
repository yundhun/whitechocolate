
bid_insert_db <- function(bid){

  sql_str <- paste0("INSERT INTO transactions (id, tr_datetime, stock_name, volume, price) VALUES('",
                    bid$'id',"','",
                    bid$'tr_datetime',"','",
                    bid$'stock_name',"',",
                    bid$'volume',",",
                    bid$'price',")")
  
  con <- dbConnect(SQLite(),dbname = "./db/wc.db")
  rs <- dbSendQuery(con,sql_str)
                           
                           
  dbClearResult(rs)
  dbDisconnect(con)
  
  }
