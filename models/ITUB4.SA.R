# Function to calculate Distance-to-default
# mcap: market value of the equity of the firm
# vol: volatility
# r: annualized interest rate
# debt: face value of its debt

#Itaú Unibanco Holding S.A. 

library(quantmod)
library(tidyverse)
library(rugarch)
library(rmgarch)
library(zoo)
library(forecast)

ENV.New <- new.env()
tickers <- c('ITUB4.SA')

Symbols <- getSymbols(Symbols = tickers, src = 'yahoo', 
                      from = "2021-01-01", 
                      to = "2022-12-31",
                      env = ENV.New)


exchange_rates <- c('USDARS=X')

Exchange_Rates <- getSymbols(Symbols = exchange_rates, src = 'yahoo', 
                             from = "2021-01-01", 
                             to = "2022-12-31",
                             auto.assign = TRUE, 
                             env = ENV.New)

Adjusted_Stock_Prices <- do.call(merge, eapply(env = ENV.New, Ad))

nonTradingDays <- apply(Adjusted_Stock_Prices[, -1], 1, function(x) any(is.na(x)))

cleanedData <- Adjusted_Stock_Prices[!nonTradingDays, ]

cleanedData <- na.locf(cleanedData)


cleanedData$ITUB4.SA.Adjusted <- cleanedData$ITUB4.SA.Adjusted/ cleanedData$USDARS.X.Adjusted


log_returns <- diff(log(cleanedData)) 
log_returns <- na.omit(log_returns) 
AvgRet <- colMeans(log_returns)
log_returns_demean <- sweep(x = log_returns, MARGIN = 2, STATS = AvgRet)

equity_returns <- log_returns_demean[,2]

auto.arima(log_returns_demean[,2]) 

GARCH_1_1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)) 

GARCH_1_1_fit <- ugarchfit(spec = GARCH_1_1, data = log_returns_demean[,2], solver = 'hybrid') 
hGARCH_1_1 <- xts(x = GARCH_1_1_fit@fit$var, order.by = index(log_returns_demean)) 
sigmaGARCH_1_1 <- xts(x = GARCH_1_1_fit@fit$sigma, order.by = index(log_returns_demean))

GARCH_1_1_fit 

income <- c("84.06M", "30.12B", "251.29M")


mcap <- 31300300000
debt <- 427962000
vol <- GARCH_1_1_fit@fit$sigma
r <- 0.15


rho <- 1                 
Maturity <- 1

seed.V <- mcap + debt

seed.sV <- matrix(0 ,length(vol), 1)

for (i in 1:length(index(vol))) {
  seed.sV[i] <- mcap * vol[i] / debt
}

debt <- debt * exp(-r)

d1 <- function(V, debt, sV, Maturity) {
  num <- log(V/debt) + 0.5*sV*sV*Maturity
  den <- sV * sqrt(Maturity)
  num/den
}

d2 <- function(V, debt, sV, Maturity) {
  d1(V, debt, sV, Maturity) - sV*sqrt(Maturity)
}


objective.function <- function(x, mcap, vol, debt, rho, Maturity) {
  e1 <- -mcap + x[1] * pnorm(d1(x[1], debt * rho, x[2], Maturity)) - rho * debt * pnorm(d2(x[1], rho * debt, x[2], Maturity))
  
  e2 <- -vol * mcap + x[2] * x[1] * pnorm(d1(x[1], debt * rho, x[2], Maturity))
  
  objective <- (e1^2) + (e2^2)
  
  return(objective)
}


res <-  list( )
for (i in 1:length(vol)) {
  res[[i]] <- optim(
    c(seed.V, seed.sV[i]), 
    method = "L-BFGS-B",
    fn = objective.function,
    lower = c(mcap, 0),
    upper = c(Inf, Inf),
    mcap = mcap,
    vol = vol[i],  
    debt = debt,
    Maturity = Maturity,
    rho = rho
  )
}

dtd.v_ITUB <- matrix(0 ,length(res)-9, 1)
for (i in 1:length(dtd.v_ITUB)) {
  dtd.v_ITUB[i] <- (res[[i]]$par[1] - debt)/(res[[i]]$par[2]*res[[i]]$par[2])
}

print(dtd.v_ITUB)

dim(dtd.v_ITUB)
