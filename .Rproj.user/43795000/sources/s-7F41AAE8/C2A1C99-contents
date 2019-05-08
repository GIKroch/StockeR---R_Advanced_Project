get_crypto <- function(tickers, start_date = Sys.Date()-365, end_date=Sys.Date()){
  
  if (!require("quantmod")) {
    install.packages("quantmod")
  }
  if(!require("httr")) {
    install.packages("httr")
  }
  if(!require("jsonlite")) {
    install.packages("jsonlite")
  }
  if(!require("anytime")) {
    install.packages("anytime")
  }
  
  start_date_num <- as.numeric(as.POSIXct(start_date, tz="GMT"))
  end_date_num <- as.numeric(as.POSIXct(end_date, tz="GMT"))
  seconds_in_day <- 60*60*24
  number_of_days <- (end_date_num - start_date_num)/seconds_in_day+1
  max_query_size <- 2000
  
  output <- data.frame()
  
  for (ticker in tickers){
    
    ticker_output <- data.frame()
    query_end_date <- end_date_num
    
    for (i in 1:ceiling(number_of_days/max_query_size)){
      query_end_date <- end_date_num - ((i-1)*max_query_size*seconds_in_day)
      query_limit <- min((query_end_date-start_date_num)/seconds_in_day, max_query_size)
      query <- paste("https://min-api.cryptocompare.com/data/histoday?fsym=", ticker, "&tsym=USD&limit=", query_limit, "&e=CCCAGG&toTs=", query_end_date, sep = "")
      result_from_query <- fromJSON(query)
      if (result_from_query$Response != "Error"){
        data <- result_from_query$Data[, c("time", "close")]
        data$time <- anydate(data$time, tz="GMT")
        data$close[data$close==0] <- NA
        ticker_output <- rbind(ticker_output, data)
      }
    }
    
    if (length(ticker_output)!=0){
      ticker_output <- unique(ticker_output)
      ticker_output <- ticker_output[order(ticker_output$time),] 
      rownames(ticker_output) <- ticker_output$time
      ticker_output$time <- NULL
      colnames(ticker_output) <- ticker
      if (length(output)==0){
        output <- ticker_output
      } else{
        output <- merge(output, ticker_output, by="row.names",all.x=TRUE)
        rownames(output) <- output$Row.names
        output$Row.names <- NULL
      }
    }
    
  }
  return(output)
  
}


## TEST
# my_data <- get_crypto(c("BTC", "ETH", "blabla", "XRP"), start_date='2015-01-01')
