## Brazil vs Argentina

library(quantmod)
library(tidyverse)
library(rugarch)
library(rmgarch)
library(zoo)
library(forecast)


### In-sample DCC Estimation Brazil vs Argentina  ------------------------------------------
rm(list = ls())
ENV.New <- new.env()
tickers <- c('BBAS3.SA','YPFD.BA')

# Get data from yahoo finance 
Symbols <- getSymbols(Symbols = tickers, src = 'yahoo', 
                      from = "2018-01-01", 
                      to = "2023-12-31",
                      env = ENV.New)


exchange_rates <- c('USDBRL=X','USDARS=X')

# Get exchange rate data
Exchange_Rates <- getSymbols(Symbols = exchange_rates, src = 'yahoo', 
                             from = "2018-01-01", 
                             to = "2023-12-31", 
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
cleanedData$BBAS3.SA.Adjusted <- cleanedData$BBAS3.SA.Adjusted/ cleanedData$USDBRL.X.Adjusted


# Convert into returns
log_returns <- diff(log(cleanedData)) # Comppute daily log returns
log_returns <- na.omit(log_returns) # Remove rows containing na's
AvgRet <- colMeans(log_returns)
log_returns_demean <- sweep(x = log_returns, MARGIN = 2, STATS = AvgRet)

## Check suggested arma_order 
# Company 1 
auto.arima(log_returns_demean[,3]) # YPFD.BA
# Company 2 
auto.arima(log_returns_demean[,4]) #BBAS3.BA
# Exchange Rate 
auto.arima(log_returns_demean[,1]) # ARS
auto.arima(log_returns_demean[,2]) # BRL



arima_orders <- list(
  c(0,0,0),                         # referencing arima orders from above
  c(0,0,0)
)


# Create a list of ugarchspec specifications with ARIMA for the mean model

uspec_list <- lapply(arima_orders, function(order) {
  ugarchspec(
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1,1)),  
    mean.model = list(armaOrder = c(order[1], order[3]), include.mean = TRUE),  # ARIMA(p,0,q) since d=0 for GARCH
    distribution.model = "std"  # std for t-distribution
  )
})

# Replicate it into a multispec() element
uspec = multispec(uspec_list)


# Define the specification for the DCC model
spec = dccspec(
  # GARCH specification
  uspec = uspec, 
  # DCC specification
  dccOrder = c(1, 1),
  # Distribution, here multivariate normal
  distribution = 'mvnorm')

# Fit the specification to the data
res <- dccfit(spec, data = log_returns_demean[,3:4])
# In sample conditional covariance
H <- res@mfit$H
#Output
res


# In sample conditional correlations

DCCrho=xts(vector(length=dim(log_returns_demean)[1]), order.by = index(log_returns_demean))
for(i in 1:dim(log_returns_demean)[1]){
  DCCrho[i] =  H[1,2,i]/sqrt(H[1,1,i]*H[2,2,i])
}

Y <- dim(log_returns_demean)[1]
X <- dim(log_returns_demean)[2] + dim(log_returns_demean)[2] * (dim(log_returns_demean)[2] - 1) / 2
vcvDCC <- xts(matrix(nrow = Y, ncol = X), order.by = index(log_returns_demean))
vcvDCC[, 1] <- sqrt(H[ 1, 1,])                      # volatility PETR3
vcvDCC[, 2] <- DCCrho                               # pairwise correlation estimate 
vcvDCC[, 3] <- sqrt(H[ 2, 2,])                     # volatility BBA3 

plot(x = index(DCCrho), y = DCCrho, type = 'l', main = 'DCC Correlation', xlab = 'Trading Days', ylab = 'Correlation')
plot(res,which=4)

## So our in-sample conditional correlation estimate is 
vcvDCC[, 2] <- DCCrho   

summary(DCCrho)
