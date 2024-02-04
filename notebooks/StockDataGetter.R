# Function to get the returns of a company

# Libraries
library(quantmod)
library(zoo)

# Function body
get_stock_returns <- function(ticker) {
  # Get Stock Data
  ENV.New <- new.env()
  stock <- ticker
  stock_name <- gsub("\\..*", "", stock)
  
  # Get data from Yahoo Finance
  Symbols <- getSymbols(Symbols = stock, src = 'yahoo', 
                        from = "2020-01-01",
                        to = "2023-12-31",
                        env = ENV.New)
  
  # Extract adjusted prices
  Adjusted_Stock_Prices <- Ad(get(ticker))
  
  # Convert into returns
  log_returns <- diff(log(Adjusted_Stock_Prices))
  log_returns <- na.omit(log_returns)
  AvgRet <- colMeans(log_returns)
  log_returns_demean <- sweep(x = log_returns, MARGIN = 2, STATS = AvgRet)
  
  # Convert xts object to a data frame
  LogReturns <- data.frame(Date = index(log_returns_demean), log_returns_demean)
  
  # Write to CSV, without adding row names
  write.csv(LogReturns, paste("~/Presentation/logreturns_", stock_name, ".csv", sep = ""), row.names = FALSE)
  
  # Get Exchange Rate Data
  ENV.2 <- new.env()
  exchange_rate <- 'USDARS=X'
  
  # Get exchange rate data
  Exchange_Rates <- getSymbols(Symbols = exchange_rate, src = 'yahoo', 
                               from = "2020-01-01", 
                               to = "2023-12-31", 
                               auto.assign = TRUE, 
                               env = ENV.2)
  
  Adjusted_ExchangeRates <- Ad(get(exchange_rate))
  Adjusted_ExchangeRates <- na.locf(Adjusted_ExchangeRates)
  
  # Merging the datasets based on their dates
  merged_data <- merge(Adjusted_Stock_Prices, Adjusted_ExchangeRates, by = "Date", all = TRUE)
  
  # Handling missing values
  merged_data <- merged_data[, !colnames(merged_data) %in% c('by')]
  
  # Adjusting the stock prices
  merged_data[[stock_name]] <- merged_data[[stock_name]] / merged_data$USDARS.X.Adjusted
  
  # Convert into returns
  log_returns_USD <- diff(log(merged_data))
  log_returns_USD <- na.omit(log_returns_USD)
  AvgRet_USD <- colMeans(log_returns_USD)
  log_returns_demean_USD <- sweep(x = log_returns_USD, MARGIN = 2, STATS = AvgRet_USD)
  log_returns_demean_USD <- log_returns_demean_USD[,1:2]
  
  # Convert xts object to a data frame
  LogReturns_USD <- data.frame(Date = index(log_returns_demean_USD), log_returns_demean_USD)
  
  # Write to CSV, without adding row names
  write.csv(LogReturns_USD, paste("~/Presentation/logreturns_", stock_name, "_USD.csv", sep = ""), row.names = FALSE)
}

# Example usage:
get_stock_returns("BBV.BA")
