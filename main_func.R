
setwd("C:/Users/user/Desktop/studia/Data Science/Semestr 2/R_Advanced/Projekt/StockeR---R_Advanced_Project")
source('download_data.r')
source('analysis.r')

## POBRANIE DANYCH - TESTY
# TEST 1
my_data <- get_data(list(c("AAPL", "MSFT", "PYPL"), c("BTC", "ETH")),  type = c("stocks", "crypto"))

# TEST 2
my_data2 <- get_data(list(c("APPL", "MICROSOFT", "PYPL")),  type = c("stocks"))
# KK: TUTAJ NAZWY KOLUMN WG MNIE LEPIEJ JAKBY BYŁY "AAPL", "MSFT", "PYPL", A NIE TE NIEPRAWDIŁOWE
# G: Powinno działać

my_data2 <- get_data(list(c("APPL", "dfgdfgdfgdfgdfgdfgrtyfhbv", "PYPL")),  type = c("stocks"))
# KK: NIE DZIAŁA
# G: Powinno działać

# TEST 3
my_data3 <- get_data(list(c("BTCOIN", "ETH", "xfgdg", "DOGE")),  type = c("crypto"))


## ANALIZA DANYCH - TESTY


results_v1 <- do_analysis_v1(list(c("AAPLLL", "MICROSOFT", "PYPL"), c("BTC", "ETH")), 
                             type = c("stocks", "crypto"), 
                             measures = c("mean", "median", "max", "min", "sd", "sd_ann", "ror"), plot = 1, save = 1)

results_v2 <- do_analysis_v2(list(c("AAPL", "MSFT", "PYPL"), c("BTC", "ETH")), 
                             type = c("stocks", "crypto"), 
                             measures = c("mean", "median", "max", "min", "sd", "sd_ann", "ror"), plot = 1, save = 0)
