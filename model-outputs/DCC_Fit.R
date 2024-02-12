library(quantmod)
library(tidyverse)
library(rugarch)
library(rmgarch)
library(zoo)
library(xts)

rm(list=ls())

BBVA <- read.csv('~/Presentation/forecast_BBV_Banco.csv', header= TRUE)
YPFD <- read.csv('~/Presentation/forecast_YPF_Energia.csv', header= TRUE)

log_returns <- merge(YPFD,BBVA, by='Date')


# Converting the Date column to Date type
log_returns$Date <- as.Date(log_returns$Date, format = "%Y-%m-%d")
log_returns <- as.xts(log_returns)

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
res <- dccfit(spec, data = log_returns)
# In sample conditional covariance
H <- res@mfit$H
#Output
res


# In sample conditional correlations

DCCrho=xts(vector(length=dim(log_returns)[1]), order.by = index(log_returns))
for(i in 1:dim(log_returns)[1]){
  DCCrho[i] =  H[1,2,i]/sqrt(H[1,1,i]*H[2,2,i])
}

Y <- dim(log_returns)[1]
X <- dim(log_returns)[2] + dim(log_returns)[2] * (dim(log_returns)[2] - 1) / 2
vcvDCC <- xts(matrix(nrow = Y, ncol = X), order.by = index(log_returns))
vcvDCC[, 1] <- sqrt(H[ 1, 1,])                      # volatility MXX
vcvDCC[, 2] <- DCCrho                               # pairwise correlation estimate 
vcvDCC[, 3] <- sqrt(H[ 2, 2,])                     # volatility BSL 

plot(x = index(DCCrho), y = DCCrho, type = 'l', main = 'DCC Correlation', xlab = 'Trading Days', ylab = 'Correlation')
plot(res,which=4)

## So our in-sample conditional correlation estimate is 
vcvDCC[, 2] <- DCCrho  


