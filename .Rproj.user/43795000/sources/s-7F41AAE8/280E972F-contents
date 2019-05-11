## This part of code deals with extracting data from yahoo
## The idea is to insert the tickers Code (tickers variable) into getSymbols. However it will only work if the code is correct
## If it is not the error functions handles it, using API which uses the user's input to get short list of companies' names/tickerss 
## which are closest to the user's initial input
get_yah <- function(tickers, start_date, end_date, apikey){
  
  ## The apikey shouldn't be visible in the function (maybe in this case it is not so crucial cause
  ## this API is completely free and its just email to get it) anyway API should be hidden somewhere
  ## my idea is to create pathToApi in function options. However i will complete this later. 
  
  if (!require("quantmod")) {
    install.packages("quantmod")
  }
  if(!require("httr")) {
    install.packages("httr")
  }
  if(!require("jsonlite")) {
    install.packages("jsonlite")
  }
  
  
  for (tick in tickers){
    if (exists("lyst") == F) {
      lyst = list()
    }
    
    err <- function(tick){
      out <- tryCatch(
      values <- c(suppressWarnings(getSymbols(as.character(tick), src = "yahoo",
                                                  verbose = F,
                                                  auto.assign = FALSE, from = as.Date(start_date), 
                                                  to = as.Date(end_date))))
      , 
      error = function(e){
       
        symb <- suppressMessages(fromJSON(content(GET(paste("https://www.alphavantage.co/query?function=SYMBOL_SEARCH&keywords=",
                                                            tick, "&apikey=",as.character(apikey), sep = "")), 
                                                  "text"), flatten = T))
        if (length(symb$bestMatches) != 0){
          alternative_symbols <- as.data.frame(symb)
          alternative_symbols <- alternative_symbols[,c(2,1)]
          colnames(alternative_symbols) <- c("Company Name", "tickerss")
          message(paste0("Sorry, the company name or its tickers you have provided is not correct.", 
                         "\n", 
                         "Below is the list with Company Names/ tickerss which are closest", "\n", 
                         " to what you have inserted into function"))
          print(alternative_symbols)
          
          message(paste0("\n","Please choose the correct tickers with the number attached to it.","\n",
                         "If you want to exit the function insert 0."))
          
          x <- readline(prompt = "Enter the number: ")
          
          
          if (x == 0) print("See you again")
          
          else {
            tick <- alternative_symbols[x,2]
            values <- c(suppressMessages(getSymbols(as.character(tick), src = "yahoo",
                                             verbose =  F,
                                             auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date))))
          }
        }
        else{
          warning(paste("Sorry we couldn't recognize the tickers you have provided: ",tick,". It was ommited"))
          # KK: JEŚLI JEST JAKIŚ BŁĄD, TO POTEM FUNKCJA NIE MOŻE PRZYPISAĆ NAZW KOLUMN POPRAWNIE (colnames(dx) <- company_tickers) - TO JEST DO POPRAWKI
        }
        
      }, 
        finally = function(f){
          lyst[[tick]] <- values
      }
      )
      return(out)
    }
    lyst[[tick]] <- err(tick)

}
  
  
  dx <- as.data.frame(lyst)
  company_tickers <- names(lyst)
  
  
  lyst <- list()
  i = 1
  
  for (colname in colnames(dx)){
    if (grepl("Close", colname) == T){
      lyst[i] <- colname
      i = i + 1
    }
  }
  dx <- dx %>% select(c(unlist(lyst)))
  colnames(dx) <- company_tickers
  
  
  return(dx)
  
  ## dx is the final output with stock prices
}


## This part of code deals with extracting data from cryptocompare
get_crypto <- function(tickers, start_date, end_date){
  
  # Load required packages
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
  if(!require("purrr")) {
    install.packages("purrr")
  }
  if(!require("plyr")) {
    install.packages("plyr")
  }
  if(!require("stringdist")){
    install.packages("stringdist")
  }
  
  # Prepare input variables
  start_date_num <- as.numeric(as.POSIXct(start_date, tz="GMT"))
  end_date_num <- as.numeric(as.POSIXct(end_date, tz="GMT"))
  seconds_in_day <- 60*60*24
  number_of_days <- (end_date_num - start_date_num)/seconds_in_day+1
  max_query_size <- 2000  # the max query size in the CyrptoCompare API is 2000 
  
  
  output <- data.frame() # output will be the final dataframe that is recurred 
  
  # For loop for all tickers entered by the user
  for (j in 1:length(tickers)){
    ticker <- tickers[j]
    ticker_output <- data.frame()
    query_end_date <- end_date_num
    
    i <- 1
    while (i <= ceiling(number_of_days/max_query_size)){
      query_end_date <- end_date_num - ((i-1)*max_query_size*seconds_in_day)
      query_limit <- min((query_end_date-start_date_num)/seconds_in_day, max_query_size) 
      query <- paste("https://min-api.cryptocompare.com/data/histoday?fsym=", ticker, "&tsym=USD&limit=", query_limit, "&e=CCCAGG&toTs=", query_end_date, sep = "")
      result_from_query <- fromJSON(query)
      
      # Check if data was downloaded with success
      if (result_from_query$Response != "Error"){
        data <- result_from_query$Data[, c("time", "close")]
        data$time <- anydate(data$time, tz="GMT")
        data$close[data$close==0] <- NA
        ticker_output <- rbind(ticker_output, data)
        i <- i+1
      } else {
        # If there is an error downloading the data, maybe the ticker was incorrect? 
        # Try finding the correct ticker
        
        # download a list of all available coins
        query_all_coins <- "https://min-api.cryptocompare.com/data/all/coinlist"
        result_from_query <- fromJSON(query_all_coins)
        CoinNamesList <- map(result_from_query$Data, 'CoinName')
        CoinNamesDf <- ldply(CoinNamesList, data.frame)
        colnames(CoinNamesDf) <- c("Symbol", "CoinName")
        
        CoinSymbolsDf <- data.frame(CoinNamesDf$Symbol, CoinNamesDf$Symbol)
        colnames(CoinSymbolsDf) <- c("Symbol", "CoinName")
        
        CoinNamesDf <- rbind(CoinNamesDf, CoinSymbolsDf)
        CoinNamesDf$CoinName <- toupper(CoinNamesDf$CoinName)
        
        # check if ticker is in the all tickers list - if yes, CryptoComparejust does not provide data for that currency
        if (is.element(ticker, CoinNamesDf$Symbol)){
          message(paste0("Sorry, data for ticker '", ticker, "' is unavailable.", 
                         "\n", 
                         "It will not be loaded."))
          i <- ceiling(number_of_days/max_query_size)
        } else{
          
          # find the closest ticker to the one that was entered
          idx_alternative_symbol <- amatch(toupper(ticker), CoinNamesDf$CoinName, maxDist=Inf, nomatch = 0)
          
          if (idx_alternative_symbol==0){
            message(paste0("Sorry, data for ticker '", ticker, "' is unavailable and there is no similar Cryptocurrency.", 
                           "\n", 
                           "It will not be loaded."))
            i <- ceiling(number_of_days/max_query_size)
            
          } else{
            alternative_symbols <- CoinNamesDf$Symbol[idx_alternative_symbol]
            
            message(paste0("Sorry, the cryptocurrency ticker ", ticker, " you have provided is not correct.", 
                           "\n", 
                           "Below there is a ticker which is the closest to what you entered"))
            
            print(alternative_symbols)
            
            message(paste0("\n","If you want to download the data for the ticker above, please insert 'Y'","\n",
                           "If you want to omit this ticker, insert anything else."))
            
            x <- readline(prompt = "Enter the answer: ")
            
            if (toupper(x) == 'Y') {
              ticker <- alternative_symbols
              i <- 1
            } else {
              i <- ceiling(number_of_days/max_query_size)+1 # exit the loop
            }
          }
          
        }
        
        
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


## Wrapper
get_data <- function(tickers, type, start_date = Sys.Date()-365, end_date=Sys.Date(), apikey = "ZFCGYJRLIXJQYHXB"){

  close_prices <- list()
  
  if("stocks" %in% type){
    tickers_stocks <- tickers[[which(type == "stocks")]]
    stock_prices <- get_yah(tickers_stocks, start_date, end_date, apikey)
    close_prices[["stocks"]] <- stock_prices
  }
  
  if("crypto" %in% type){
    tickers_crypto <- tickers[[which(type == "crypto")]]
    crypto_prices <- get_crypto(tickers_crypto, start_date, end_date)
    close_prices[["crypto"]] <- crypto_prices
  }
  
  return(close_prices)
  
}
