
# loading quantmod library 
library(quantmod)

# load other libraries to help with data preparation
library(zoo)


## Get Stock Data

ENV.New <- new.env()     # Create enviroment of where data is stored 
tickers <- c('BBV.BA','YPFD.BA')    # Example ticker : Banco Bilbao Vizcaya Argentaria(Argentina)(finance)
# YPF(Argentina)(Energy)
stocks <- c('BBV.Banco', 'YPF.Energia')

# Optional code to get clean column names; useful when extracting tickers >= 2  
tickers_cleaned <- c('BBV.BA', 'YPFD.BA')
tickers_cleaned <- as.vector(sapply(tickers_cleaned, FUN = function(x) paste(x, '.Adjusted', sep = '')))
# to restore order of columns violated by the do.call merge function below 

# Get data from yahoo finance 
Symbols <- getSymbols(Symbols = tickers, src = 'yahoo', 
                      from = "2020-01-01",  # specify a start date
                      to = "2023-12-31",    # specify a end date
                      env = ENV.New)        # Enviroment to store the data



# Create one XTS object containing adjusted prices of all stocks
Adjusted_Stock_Prices <- do.call(merge, eapply(env = ENV.New, Ad)) 
# Here we collect adjusted stock prices and store them in our env
# Ad extracts adjusted prices for every stock. Consequently, all adjusted prices are merged into one xts object
Adjusted_Stock_Prices <- Adjusted_Stock_Prices[, tickers_cleaned]  # Restore the right order of columns

# assign stock names to column names
names(Adjusted_Stock_Prices) <- stocks

# Apply na.locf to fill in missing values in each dataset by carrying forward previous values  
Adjusted_Stock_Prices <- na.locf(Adjusted_Stock_Prices)
# LAC has many non-trading days. 

# Convert into returns
log_returns <- diff(log(Adjusted_Stock_Prices)) # Compute daily log returns
log_returns <- na.omit(log_returns) # Remove rows containing na's
AvgRet <- colMeans(log_returns)     # Note this syntax is used for multiple stocks; for single stock we would use mean 
log_returns_demean <- sweep(x = log_returns, MARGIN = 2, STATS = AvgRet) # Single: log_returns - AvgReturns 

# Convert xts object to a data frame
LogReturns <- data.frame(Date = index(log_returns_demean), log_returns_demean)

# Write to CSV, without adding row names
write.csv(LogReturns, '~/Presentation/logreturnsArgentina2.csv', row.names = FALSE) #specify directory for saving csv file 
# example : Mac ~/Folder/'filename.csv' , here we do not want to add row names  



## Currency Adjustment -------------------------------------------------------------------------------------------------------------------

# we need currencies of our data to match up to make useful comparisons. 

ENV.2 <- new.env()
exchange_rates <- c('USDARS=X')

# Get exchange rate data
Exchange_Rates <- getSymbols(Symbols = exchange_rates, src = 'yahoo', 
                             from = "2020-01-01", 
                             to = "2023-12-31", 
                             auto.assign = TRUE, 
                             env = ENV.2)

Adjusted_ExchangeRates <- do.call(merge, eapply(env = ENV.2, Ad))
Adjusted_ExchangeRates <- na.locf(Adjusted_ExchangeRates)

# Merging the datasets based on their dates
merged_data <- merge(Adjusted_Stock_Prices, Adjusted_ExchangeRates, by = "Date",all = TRUE)


# Handling missing values (e.g., using na.locf from zoo package)
merged_data <- merged_data[, !colnames(merged_data) %in% c('by')]


# Adjusting the stock prices. Here we need to adjust for each asset 
merged_data$BBV.Banco <- merged_data$BBV.Banco / merged_data$USDARS.X.Adjusted
merged_data$YPF.Energia <- merged_data$YPF.Energia / merged_data$USDARS.X.Adjusted

# Convert into returns
log_returns_USD <- diff(log(merged_data)) # Compute daily log returns
log_returns_USD <- na.omit(log_returns_USD) # Remove rows containing na's
AvgRet_USD <- colMeans(log_returns_USD)
log_returns_demean_USD <- sweep(x = log_returns_USD, MARGIN = 2, STATS = AvgRet_USD)
log_returns_demean_USD <- log_returns_demean_USD[,1:2]

# Convert xts object to a data frame
LogReturns_USD <- data.frame(Date = index(log_returns_demean_USD), log_returns_demean_USD)

# Write to CSV, without adding row names
write.csv(LogReturns, '~/Presentation/logreturnsArgentina2_USD.csv', row.names = FALSE) #specify directory for saving csv file 
# example : Mac ~/Folder/'filename.csv' , here we do not want to add row names  


