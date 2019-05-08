do_analysis_v1 <- function(tickers, type, start_date = Sys.Date()-365, end_date=Sys.Date(), measures = NULL, plot = 0, save = 0){
  
  if(!require("dplyr")) {
    install.packages("dplyr")
  }
  
  
  ####### Loading data and assertion is the same for do_analysis_v1 and do_analysis_v2 #######
  # assert start_date and end_date are Dates
  stopifnot(class(start_date)=="Date"&class(end_date)=="Date")
  stopifnot(start_date<end_date)
  
  
  close_prices <- get_data(tickers, type, start_date, end_date)
  
  ## Measures for stocks. It looks short and easy but it is actually pretty complicated. 
  ## Few operations wrapped.
  ### Defining mode function
  
  if(exists("measures") == T){
    
    # supported measures
    measures_on_returns <- c("mean", "median", "max", "min", "sd", "sd_ann")
    measures_on_prices <- c("ror")
    measures_all <- c(measures_on_returns, measures_on_prices)
    
    # assert every chosen measure is supported
    stopifnot(all(is.element(measures, measures_all)))
    
    ####### Calculating measures - using apply #######
    
    measures_func <- function(y, type){
      
      sd_ann <- function(y){
        if (type=="stock") 
        {D=252} else if (type=="crypto") 
        {D=365}
        std_ann <- sd(y)*sqrt(D)
      }
      
      n <- length(y)
      returns <- y[(n-1):1] / y[n:2] - 1
      
      outcome1 <- sapply(measures[is.element(measures, measures_on_returns)], function(x){
        get(x)(returns)
      })
      
      outcome2 <- sapply(measures[is.element(measures, measures_on_prices)], function(x){
        get(x)(y)
      })
      
      outcome <- c(outcome1, outcome2)
      
    }
    
    stock_measures <- as.data.frame(sapply(close_prices[["stocks"]], measures_func, type="stock"))
    crypto_measures <- as.data.frame(sapply(close_prices[["crypto"]], measures_func, type="crypto"))
    close_prices[["stock_measures"]] <- stock_measures
    close_prices[["crypto_measures"]] <- crypto_measures
    
  }
  
  ## Line plot for prices
  if (plot == 1){
    
    if(!require("ggplot2")){
      install.packages("ggplot2")
    }
    
    if(!require("reshape2")){
      install.packages("reshape2")
    }
    
    
    lineplot_data <- c()
    barplot_data <- c()
    for (name in names(close_prices)){
      if (name == "crypto" | name == "stocks"){
        lineplot_data <- c(lineplot_data,name)
      }
      else{
        barplot_data <- c(barplot_data, name)
      }
    }
    
    
    for(name in lineplot_data){
      # close_prices[[name]][["dates"]] <- row.names(close_prices[[name]])
      close_prices[[name]][["dates"]] <- as.Date(row.names(close_prices[[name]]), format = "%Y-%m-%d")
      
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
      
      line_plots <- lapply(names(close_prices[[name]][1:length(close_prices[[name]])-1]), 
                           function(y) {ggplot(close_prices[[name]], aes(x = dates, y = get(y))) + 
                               geom_line(group = 1) + 
                               ggtitle(as.character(y)) + 
                               ylab("price") +
                               scale_x_date(breaks = seq(as.Date(start_date), 
                                                         as.Date(end_date), by=by), 
                                            date_labels = "%Y-%m-%d")})
      
      ### The for loop below is supposed to do the same task as the sapply after it. However
      ### For some reasons this for loop doesn't produce all plots it should. 
      
      # for(i in range(1:length(line_plots))){
      #   title <- line_plots[[i]][["labels"]][["title"]]
      #   png(filename=paste0(title,"_line_plot.png"), width = 1600, height = 980 )
      #   invisible(print(line_plots[[i]]))
      #   dev.off()
      # }
      #
      
      sapply(line_plots, function(x){
        title <- x[["labels"]][["title"]][1]
        png(filename=paste0(title,"_line_plot.png"), width = 1600, height = 980 )
        invisible(print(x))
        dev.off()
      })
      
      close_prices[[paste0(name,"_", "line_plots")]] <- line_plots
      close_prices[[name]][["dates"]] <- NULL
      
      
    }
    
    for(name in barplot_data){
      ## Measure barplots
      
      mtab <- melt(as.matrix(close_prices[[name]]))
      measure_plots <- ggplot(data = mtab, aes(x= Var1, y = value, fill = Var1)) + 
        geom_bar(stat = "identity") +
        scale_fill_viridis_d() +
        facet_grid(. ~ Var2) + 
        labs(fill = "Measures") + 
        theme(axis.title.x = element_blank())
      
      if(save == 1){
        ggsave(paste0(name,"_", "barplot",".png"),plot = measure_plots)
      }
      
      close_prices[[paste0(name,"_", "barplot")]] <- measure_plots
    }
    
    
  }
  ##
  
  ## Saving all data_frames to csv
  if(save == 1){
    for (name in names(close_prices)){
      if (class(close_prices[[name]]) == "data.frame"){
        write.csv(close_prices[name],paste0(name,".csv"))
      }
    }
  }
  
  
  ## Returning the full output of the function
  return(close_prices)
  
}

ror <- function(y){
  n <- length(y)
  ror <- y[n] / y[1] - 1
}

ror_ann <- function(y, type){
  if (type=="stock") D=252
  else if (type=="crypto") D=365
  n <- length(y)
  ror <- y[n] / y[1] - 1
  ror_ann <- (1+ror)^(D/length(y))
}

