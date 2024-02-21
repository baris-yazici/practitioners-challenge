# Load the quantmod library
library(quantmod)

# Create a new environment
ENV.Oil <- new.env()

# Define the ticker symbol for Crude Oil
oil_ticker <- 'CL=F'

# Get data for Crude Oil Apr 24 from Yahoo Finance
getSymbols(Symbols = oil_ticker, src = 'yahoo', 
           from = "2018-01-03", 
           to = "2023-12-28", 
           auto.assign = TRUE, 
           env = ENV.Oil)

# Extract adjusted prices for Crude Oil
Adjusted_Oil_Prices <- Ad(ENV.Oil[[oil_ticker]])

# Convert xts object to a data frame
Oil_Data <- data.frame(Date = index(Adjusted_Oil_Prices), Prices = coredata(Adjusted_Oil_Prices))

# Write to CSV, without adding row names
write.csv(Oil_Data, file = "oil_data.csv", row.names = FALSE)
