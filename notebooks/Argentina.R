## Brazil vs Argentina

library(quantmod)
library(tidyverse)
library(rugarch)
library(rmgarch)
library(zoo)
library(forecast)


### In-sample DCC Estimation Brazil   ------------------------------------------
rm(list = ls())
ENV.New <- new.env()
tickers <- c('YPFD.BA')

# Get data from yahoo finance 
Symbols <- getSymbols(Symbols = tickers, src = 'yahoo', 
                      from = "2021-01-01", 
                      to = "2022-12-31",
                      env = ENV.New)


exchange_rates <- c('USDARS=X')

# Get exchange rate data
Exchange_Rates <- getSymbols(Symbols = exchange_rates, src = 'yahoo', 
                             from = "2021-01-01", 
                             to = "2022-12-31",
                             auto.assign = TRUE, 
                             env = ENV.New)

# Create one XTS object containing adjusted prices of all stocks
Adjusted_Stock_Prices <- do.call(merge, eapply(env = ENV.New, Ad))

## Data cleaning ---------------------------------------------------------------
# Step 1: Identify rows where any stock has NA values (non-trading days for the stocks)
nonTradingDays <- apply(Adjusted_Stock_Prices[, -1], 1, function(x) any(is.na(x)))

# Step 2: Remove these non-trading days from the dataset
cleanedData <- Adjusted_Stock_Prices[!nonTradingDays, ]
# Step 3 : Carry forward last values for when exchange rate markets are closed. 
cleanedData <- na.locf(cleanedData)


# Adjusting the stock prices
cleanedData$YPFD.BA.Adjusted <- cleanedData$YPFD.BA.Adjusted/ cleanedData$USDARS.X.Adjusted



# Convert into returns
log_returns <- diff(log(cleanedData)) # Comppute daily log returns
log_returns <- na.omit(log_returns) # Remove rows containing na's
AvgRet <- colMeans(log_returns)
log_returns_demean <- sweep(x = log_returns, MARGIN = 2, STATS = AvgRet)

## Check suggested arma_order 
# Company 1 
auto.arima(log_returns_demean[,2]) # YPFD.BA


