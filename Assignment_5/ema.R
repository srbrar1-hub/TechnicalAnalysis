# Assignment 5 - Exponential Moving Average (EMA)
# BDA400 - Technical Analysis Using R

ema <- function(data, period) {
  if (length(data) == 0 || period < 1) {
    stop("Data must not be empty and period must be positive")
  }
  
  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  ema_values[1] <- data[1]
  
  if (length(data) > 1) {
    for (i in 2:length(data)) {
      ema_values[i] <- (data[i] - ema_values[i - 1]) *
        multiplier + ema_values[i - 1]
    }
  }
  
  return(ema_values)
}

# Test EMA function
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
ema_result <- ema(data, period = 3)
print(ema_result)