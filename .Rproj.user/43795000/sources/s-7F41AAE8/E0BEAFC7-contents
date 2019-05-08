if(!require("httr")) install.packages("httr")
if(!require("jsonlite")) install.packages("jsonlite")


# pack <- c("httr", "jsonlite")
# lapply(pack, library, character.only = TRUE)

# API Options: function = c(CURRENCY_EXCHANGE_RATE, FX_INTRADAY, FX_WEEKLY, FX_MONTHLY
# from_symbol=EUR
# to_symbol=USD
get_currencies <- function(typeOfData = "FX_WEEKLY", fromCurrency = "PLN", toCurrency = "EUR", interval = '60min'){
  
  url_base <-  'https://www.alphavantage.co/query?function='
  
  
  if (as.character(typeOfData) == "FX_INTRADAY"){
    url <- paste(url_base,as.character(typeOfData),"&", 
          "from_symbol=", as.character(fromCurrency), "&",
          "to_symbol=",as.character(toCurrency), 
          "&interval=",as.character(interval),"&apikey=ZFCGYJRLIXJQYHXB",sep = "")
  }
  
  else{
    url <- paste(url_base,as.character(typeOfData),"&", 
                 "from_symbol=", as.character(fromCurrency), "&",
                 "to_symbol=",as.character(toCurrency), "&apikey=ZFCGYJRLIXJQYHXB", sep = "")
  }
  
  jason <- fromJSON(content(GET(url), "text"), flatten = T)
  
  
  return(jason)
  
}


