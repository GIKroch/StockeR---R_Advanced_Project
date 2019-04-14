# get_yah <- function(company, start_date, end_date){
#   for (comp in company){
#     if (exists("lyst") == F) {
#       lyst = list()
#     }
#     lyst[[comp]] <- suppressWarnings(getSymbols(as.character(comp), src = "yahoo",
#                                                 verbose = F,
#                                                 auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date)))
#     
#   }
#   
#   for (i in range(1:length(lyst))){
#     if (exists("dx") == F) {
#       dx <- as.data.frame(lyst[[i]])
#       
#     }
#     else {
#       x <- as.data.frame(lyst[[i]])
#       dx <- cbind(dx, x)
#     }
#   }
#   
#   return(dx)
# }



get_data <- function(company = "AAPL", start_date = "2016-01-01", end_date = "2016-10-01"){

  if (!require("quantmod")) {
    install.packages("quantmod")
  }
  if(!require("httr")) {
    install.packages("httr")
  }
  if(!require("jsonlite")) {
    install.packages("jsonlite")
  }
  
  
  ## The idea is to insert the Company Code (company variable) into getSymbols. However it will only work if the code is correct
  ## If it is not the error functions handles it, using API which uses the user's input to get short list of companies' names/codes 
  ## which are closest to the initial user's input
  
  x <- tryCatch({
    for (comp in company){
      if (exists("lyst") == F) {
        lyst = list()
      }
      lyst[[comp]] <- suppressWarnings(getSymbols(as.character(comp), src = "yahoo",
                                  verbose = F,
                                  auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date)))

    }

    for (i in range(1:length(lyst))){
      if (exists("dx") == F) {
        dx <- as.data.frame(lyst[[i]])

      }
      else {
        x <- as.data.frame(lyst[[i]])
        dx <- cbind(dx, x)
      }
    }

    return(dx)
    
    
    
  }, 
  error = function(e){
    
    
    symb <- suppressMessages(fromJSON(content(GET(paste("https://www.alphavantage.co/query?function=SYMBOL_SEARCH&keywords=",
                                            company, "&apikey=ZFCGYJRLIXJQYHXB", sep = "")), 
                                            "text"), flatten = T))
      
    alternative_symbols <- as.data.frame(symb)
    alternative_symbols <- alternative_symbols[,c(2,1)]
    colnames(alternative_symbols) <- c("Company Name", "Stock Market Code")
    message(paste0("Sorry, the company name or its market code you have provided is not correct.", 
                   "\n", 
                   "Below is the list with Company Names/ Stock Market Codes which are closest", "\n", 
                   " to what you have inserted into function"))
    print(alternative_symbols)
    
    message(paste0("\n","Please choose the correct company with the number attached to it.","\n",
                   "If you want to leave the function insert 0."))
    
    x <- readline(prompt = "Enter the number: ")
    
    
    if (x == 0) print("See ya again fella")
      
    else {
      company <- alternative_symbols[x,2]
      d <- suppressMessages(getSymbols(as.character(company), src = "yahoo",
                 verbose =  F,
                 auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date)))
      
      d <- as.data.frame(d)
    }
  }
  )
  
  return (x)
  
}




wuh <- get_data(c("AAPL", "MSFT"))

