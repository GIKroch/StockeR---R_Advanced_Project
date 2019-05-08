source('download_data.r')
source('analysis.r')

## POBRANIE DANYCH - TESTY
# TEST 1
my_data <- get_data(list(c("AAPL", "MSFT", "PYPL"), c("BTC", "ETH")),  type = c("stocks", "crypto"))

# TEST 2
my_data2 <- get_data(list(c("APPL", "MICROSOFT", "PYPL")),  type = c("stocks"))
# KK: TUTAJ NAZWY KOLUMN WG MNIE LEPIEJ JAKBY BYŁY "AAPL", "MSFT", "PYPL", A NIE TE NIEPRAWDIŁOWE

my_data2 <- get_data(list(c("APPL", "dfgdfgdfgdfgdfgdfgrtyfhbv", "PYPL")),  type = c("stocks"))
# KK: NIE DZIAŁA

# TEST 3
my_data3 <- get_data(list(c("BTCOIN", "ETH", "xfgdg", "DOGE")),  type = c("crypto"))


## ANALIZA DANYCH - TESTY

start_time <- Sys.time()
results_v1 <- do_analysis_v1(list(c("AAPL", "MSFT", "PYPL"), c("BTC", "ETH")), 
                             type = c("stocks", "crypto"), 
                             measures = c("mean", "median", "max", "min", "sd", "sd_ann", "ror"), plot = 0, save = 0)
end_time <- Sys.time()
end_time - start_time


results_v2 <- do_analysis_v2(list(c("AAPL", "MSFT", "PYPL"), c("BTC", "ETH")), 
                             type = c("stocks", "crypto"), 
                             measures = c("mean", "median", "max", "min", "mode"), plot = 1, save = 1)


