# Assignment 5 - Stochastic RSI
# BDA400 - Technical Analysis Using R

# Load required functions
source("rsi.R")
source("sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  
  if (k_period < 1 || d_period < 1) {
    stop("K and D periods must be positive")
  }
  
  # Calculate RSI
  rsi_values <- rsi(data, period)
  
  # Initialize Stochastic RSI values
  stoch_values <- rep(NA_real_, length(data))
  
  # Normalize RSI using rolling minimum and maximum
  for (i in seq_along(rsi_values)) {
    start <- i - k_period + 1
    
    if (start >= 1) {
      window <- rsi_values[start:i]
      
      if (all(!is.na(window))) {
        min_rsi <- min(window)
        max_rsi <- max(window)
        
        if (max_rsi == min_rsi) {
          stoch_values[i] <- 0
        } else {
          stoch_values[i] <-
            (rsi_values[i] - min_rsi) /
            (max_rsi - min_rsi)
        }
      }
    }
  }
  
  # Calculate K line
  k_line <- stoch_values
  
  # Calculate D line using SMA
  d_line <- rep(NA_real_, length(data))
  
  for (i in seq_along(k_line)) {
    start <- i - d_period + 1
    
    if (start >= 1) {
      window <- k_line[start:i]
      
      if (all(!is.na(window))) {
        d_line[i] <- sum(window) / d_period
      }
    }
  }
  
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}

# Test Stochastic RSI
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62,
          66, 68, 64, 70, 73, 69, 75, 78, 74, 80,
          77, 82, 79, 85, 83, 88, 84, 90, 87, 92)

stoch_result <- stoch_rsi(
  data,
  period = 5,
  k_period = 3,
  d_period = 3
)

print(stoch_result)