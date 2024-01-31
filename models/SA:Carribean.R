library(quantmod)
library(tidyverse)
library(rugarch)
library(rmgarch)
library(zoo)

## Getting S&PLAC40 data 
rm(list=ls())
SPLAC <- read.csv('~/Presentation/S&PLAC40.csv', header= FALSE)
SPLAC <- SPLAC[, colSums(is.na(SPLAC)) < nrow(SPLAC)]
names(SPLAC) <- c("Date", "Prices(USD)")
# Converting the Date column to Date type
SPLAC$Date <- as.Date(SPLAC$Date, format = "%d/%m/%Y")
SPLAC <- na.omit(SPLAC)

write.csv(SPLAC, '~/Presentation/dailyclosingprices.csv', row.names=FALSE)


SPLAC <- as.xts(SPLAC)
# Convert into returns
log_returnsSP <- diff(log(SPLAC)) # Comppute daily log returns
log_returnsSP <- na.omit(log_returnsSP) # Remove rows containing na's
AvgRetSP <- mean(log_returnsSP)
log_returns_demeanSP <- log_returnsSP - AvgRetSP # subtracting column means from each value in each column; log_returns - AvgRet does not work in R

# Convert xts object to a data frame
SPLAC_df <- data.frame(Date = index(log_returns_demeanSP), log_returns_demeanSP)

# Write to CSV, without adding row names
write.csv(SPLAC_df, '~/Presentation/dailylogprices.csv', row.names = FALSE)



### In-sample DCC Estimation Brazil vs Mexico.  ------------------------------------------

ENV.New <- new.env()
tickers <- c('^BVSP','^MXX')

# Get data from yahoo finance 
Symbols <- getSymbols(Symbols = tickers, src = 'yahoo', 
                      from = "2020-01-01", 
                      to = "2023-12-31",
                      env = ENV.New)


# Create one XTS object containing adjusted prices of all stocks
Adjusted_Stock_Prices <- do.call(merge, eapply(env = ENV.New, Ad))



ENV.2 <- new.env()
exchange_rates <- c('USDBRL=X', 'USDMXN=X')

# Get exchange rate data
Exchange_Rates <- getSymbols(Symbols = exchange_rates, src = 'yahoo', 
                             from = "2020-01-01", 
                             to = "2023-12-31", 
                             auto.assign = TRUE, 
                             env = ENV.2)

Adjusted_ExchangeRates <- do.call(merge, eapply(env = ENV.2, Ad))



# Apply na.locf to fill in missing values in each dataset
Adjusted_Stock_Prices <- na.locf(Adjusted_Stock_Prices)
Adjusted_ExchangeRates <- na.locf(Adjusted_ExchangeRates)


# Merging the datasets based on their dates
merged_data <- merge(Adjusted_Stock_Prices, Adjusted_ExchangeRates, by='Date')

# Handling missing values (e.g., using na.locf from zoo package)
merged_data <- na.locf(merged_data)
merged_data <- merged_data[, !colnames(merged_data) %in% c('by')]


# Adjusting the stock prices
merged_data$BVSP.Adjusted <- merged_data$BVSP.Adjusted / merged_data$USDBRL.X.Adjusted
merged_data$MXX.Adjusted <- merged_data$MXX.Adjusted / merged_data$USDMXN.X.Adjusted

# Convert into returns
log_returns <- diff(log(merged_data)) # Comppute daily log returns
log_returns <- na.omit(log_returns) # Remove rows containing na's
AvgRet <- colMeans(log_returns)
log_returns_demean <- sweep(x = log_returns, MARGIN = 2, STATS = AvgRet)

log_returns_demean <- log_returns_demean[,1:2]


# Specify the default univariate GARCH model with no mean
xspec = ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
# Replicate it into a multispec() element
uspec = multispec(replicate(2, xspec))
# Define the specification for the DCC model
spec = dccspec(
  # GARCH specification
  uspec = uspec, 
  # DCC specification
  dccOrder = c(1, 1),
  # Distribution, here multivariate normal
  distribution = 'mvnorm')

# Fit the specification to the data
res <- dccfit(spec, data = log_returns_demean)
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
vcvDCC[, 1] <- sqrt(H[ 1, 1,])                      # volatility MXX
vcvDCC[, 2] <- DCCrho                               # pairwise correlation estimate 
vcvDCC[, 3] <- sqrt(H[ 2, 2,]).                     # volatility BSL 

plot(x = index(DCCrho), y = DCCrho, type = 'l', main = 'DCC Correlation', xlab = 'Trading Days', ylab = 'Correlation')
plot(res,which=4)

## So our in-sample conditional correlation estimate is 
vcvDCC[, 2] <- DCCrho   