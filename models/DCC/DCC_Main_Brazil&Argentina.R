# DCC estimation code to use for companies in Brazil.

library(quantmod)
library(tidyverse)
library(rugarch)
library(rmgarch)
library(zoo)
library(forecast)

rm(list = ls())
ENV.New <- new.env()

# List of company tickers.
brazil <- c('POMO3.SA', 'JBSS3.SA' , 'MRFG3.SA', 'TSLA34.SA',  
            'OIBR4.SA', 'BICR11.SA', 'PETR4.SA', 'BBAS3.SA', 
            'BBDC4.SA', 'PCAR3.SA', 'TRAD3.SA', 'BRIV4.SA') 


argentina <- c('YPFD.BA', 'PAMP.BA', 'ALUA.BA', 'CELU.BA', 'GGAL.BA', 'BMA.BA') 

exchange_rates <- c('USDBRL=X','USDARS=X')

# Generate all combinations of pairs
combinations <- expand.grid(argentina, brazil)

# Loop through each row of combinations
for (i in 1:nrow(combinations)) {
  
  combination <- combinations[i, ]
  ticker_combination <- as.character(unlist(combination))
  
  tryCatch({
    # Get data from yahoo finance 
    Symbols <- getSymbols(Symbols = ticker_combination, 
                          src = 'yahoo', 
                          from = "2018-01-01", 
                          to = "2023-12-31",
                          env = ENV.New)
    
    # Get exchange rate data
    Exchange_Rates <- getSymbols(Symbols = exchange_rates, 
                                 src = 'yahoo', 
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
    cleanedData[, paste0(ticker_combination[1], ".Adjusted")] <- cleanedData[, paste0(ticker_combination[1], ".Adjusted")] / cleanedData$USDARS.X.Adjusted
    cleanedData[, paste0(ticker_combination[2], ".Adjusted")] <- cleanedData[, paste0(ticker_combination[2], ".Adjusted")] / cleanedData$USDBRL.X.Adjusted
    
    # Check if there are still NA values after removing non-trading days
    if (anyNA(cleanedData)) {
      # Skip this iteration if there are NA values
      next
    }
    
    # Convert into returns
    log_returns <- diff(log(cleanedData)) # Comppute daily log returns
    log_returns <- na.omit(log_returns) # Remove rows containing na's
    AvgRet <- colMeans(log_returns)
    log_returns_demean <- sweep(x = log_returns, MARGIN = 2, STATS = AvgRet)
    
    
    # Fitting ARIMA models to the three series
    fit_auto1 <- auto.arima(log_returns_demean[,2]) # Company 1
    fit_auto2 <- auto.arima(log_returns_demean[,3]) # Company 2
    fit_auto3 <- auto.arima(log_returns_demean[,1]) # Exchange Rate
    
    # Extracting the orders
    order_auto1 <- c(fit_auto1$arma[1], fit_auto1$d, fit_auto1$arma[6])
    order_auto2 <- c(fit_auto2$arma[1], fit_auto2$d, fit_auto2$arma[6])
    order_auto3 <- c(fit_auto3$arma[1], fit_auto3$d, fit_auto3$arma[6])
    
    arima_orders <- list(order_auto1, order_auto2)
    
    # Create a list of ugarchspec specifications with ARIMA for the mean model
    uspec_list <- lapply(arima_orders, function(order) {
      ugarchspec(
        variance.model = list(model = "gjrGARCH", garchOrder = c(1,1,1)),  
        mean.model = list(armaOrder = c(order[1], order[2]), include.mean = TRUE),  # ARIMA(p,0,q) since d=0 for GARCH
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
    
    # In sample conditional correlations
    DCCrho=xts(vector(length=dim(log_returns_demean)[1]), order.by = index(log_returns_demean))
    
    # Calculate DCCrho values
    for(i in 1:dim(log_returns_demean)[1]){
      DCCrho[i] = H[1,2,i]/sqrt(H[1,1,i]*H[2,2,i])
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
    
    # Convert the xts object to a data frame
    DCCrho_final <- data.frame(Date = index(DCCrho),
                               DCCrho = coredata(DCCrho))
    
    # Export the data frame to a CSV file
    write.csv(DCCrho_final, 
              file = paste0("DCCrho_", 
                            ticker_combination[1], 
                            "_", 
                            ticker_combination[2], ".csv"), 
              row.names = FALSE)
    
  }, error = function(e) {
    # Skip to the next iteration if there's an error
    cat("Error occurred for combination:", ticker_combination, "\n")
    return(invisible(NULL))  # Use 'return(invisible(NULL))' instead of 'next'
  })
}