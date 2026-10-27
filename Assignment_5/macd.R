
# Assignment 5 - MACD
# BDA400 - Technical Analysis Using R

# Load EMA function
source("ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  
  # Calculate short and long EMAs
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  
  # Calculate MACD line
  macd_line <- short_ema - long_ema
  
  # Calculate signal line
  signal_line <- ema(macd_line, signal_period)
  
  # Calculate histogram
  histogram <- macd_line - signal_line
  
  # Return results
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  
  return(result)
}

# Test MACD function
data <- c(100, 105, 110, 115, 120, 125, 130)

macd_result <- macd(
  data,
  short_period = 3,
  long_period = 5,
  signal_period = 2
)

print(macd_result)