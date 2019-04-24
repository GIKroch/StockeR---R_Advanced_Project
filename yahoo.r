# setwd("C:/Users/grzeg/Desktop/studia/Semestr 2/Advanced_R/R_Advanced_Project")

## The apikey shouldn't be visible in the function (maybe in this case it is not so crucial cause
## this API is completely free and its just email to get it) anyway API should be hidden somewhere
## my idea is to create pathToApi in function options. However i will complete this later.  

# txt <- suppressWarnings(readLines("Alpha_Vantage_API.txt"))


get_yah <- function(ticker, start_date, end_date, apikey){
  
  ## This part of code deals with extracting data from yahoo
  ## The idea is to insert the ticker Code (ticker variable) into getSymbols. However it will only work if the code is correct
  ## If it is not the error functions handles it, using API which uses the user's input to get short list of companies' names/tickers 
  ## which are closest to the user's initial input
  
  for (tick in ticker){
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
          colnames(alternative_symbols) <- c("Company Name", "Tickers")
          message(paste0("Sorry, the company name or its ticker you have provided is not correct.", 
                         "\n", 
                         "Below is the list with Company Names/ Tickers which are closest", "\n", 
                         " to what you have inserted into function"))
          print(alternative_symbols)
          
          message(paste0("\n","Please choose the correct ticker with the number attached to it.","\n",
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
          warning(paste("Sorry we couldn't recognize the ticker you have provided: ",tick,". It was ommited"))
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
  company_ticker <- names(lyst)
  
  
  lyst <- list()
  i = 1
  
  for (colname in colnames(dx)){
    if (grepl("Close", colname) == T){
      lyst[i] <- colname
      i = i + 1
    }
  }
  dx <- dx %>% select(c(unlist(lyst)))
  colnames(dx) <- company_ticker
  
  
  return(dx)
  
  ## dx is the final output with stock prices
}


get_data <- function(ticker, start_date = "2019-01-01", end_date = "2019-01-26", 
                     apikey = "ZFCGYJRLIXJQYHXB", measures, plot = 0){

  if (!require("quantmod")) {
    install.packages("quantmod")
  }
  if(!require("httr")) {
    install.packages("httr")
  }
  if(!require("jsonlite")) {
    install.packages("jsonlite")
  }
  if(!require("dplyr")) {
    install.packages("dplyr")
  }
  
  
  ## getting data about stock prices from yahoo
  stock_prices <- get_yah(ticker, start_date, end_date)
  ##
  
  
  ## Measures for stocks. It looks short and easy but it is actually pretty complicated. 
  ## Few operations wrapped.
  ### Defining mode function
  if ("mode" %in% measures){
    mode <- function(v) {
      uniqv <- unique(round(v))
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
  }
  ###
  measures_func <- function(y){
    list(sapply(measures, function(x){
      get(x)(y)
    }))
  }
  
  stock_measures <- as.data.frame(sapply(stock_prices, measures_func))
  ##
  
  
  ## Line plot for prices
  if (plot == 1){
    
    if(!require("ggplot2")){
      install.packages("ggplot2")
    }
    
    stock_prices["dates"] <- row.names(stock_prices)
    stock_prices["dates"] <- as.Date(stock_prices$dates, format = "%Y-%m-%d")
    
    
    numberOfDates <- as.Date(end_date) - as.Date(start_date)
    
    ### Those lines scale the x_axis labels to make the output readable
    if (numberOfDates >= 90){
      by <- "1 month"
    }
    
    else if (numberOfDates < 90 & numberOfDates > 30){
      by <- "1 week"
    }
    
    else {
      by <- "3 days"
    }
    line_plots <- lapply(names(stock_prices[1:length(stock_prices)-1]), 
                         function(y) {ggplot(stock_prices, aes(x = dates, y = get(y))) + 
                             geom_line(group = 1) + 
                             ggtitle(as.character(y)) + 
                             ylab("price") +
                             scale_x_date(breaks = seq(as.Date(start_date), 
                                                       as.Date(end_date), by=by), 
                                                       date_labels = "%Y-%m-%d")})
    
    ## Measure barplots
    stock_prices["dates"] <- NULL
    mtab <- melt(as.matrix(stock_measures))
    measure_plots <- ggplot(data = mtab, aes(x= Var1, y = value, fill = Var1)) + 
                            geom_bar(stat = "identity") +
                            scale_fill_viridis_d() +
                            facet_grid(. ~ Var2) + 
                            labs(fill = "Measures") + 
                            theme(axis.title.x = element_blank())
    print(line_plots)            
    print(measure_plots)
  }
  ##
  
  
  return(stock_prices)
  
}


stock_prices <- get_data(c("AAPL", "MSFT", "PYPL"), measures = c("mean", "max", "min", "median", "mode"), plot = 1)

