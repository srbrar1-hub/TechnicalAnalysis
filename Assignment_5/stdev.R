
# Assignment 5 - Standard Deviation
# BDA400 - Technical Analysis Using R

stdev <- function(data) {
  
  if (length(data) == 0) {
    stop("Data must not be empty")
  }
  
  # Calculate mean
  mean_value <- sum(data) / length(data)
  
  # Calculate differences from mean
  diff_values <- data - mean_value
  
  # Square the differences
  squared_diff <- diff_values^2
  
  # Calculate population variance
  variance <- sum(squared_diff) / length(data)
  
  # Calculate standard deviation
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}

# Test Standard Deviation function
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

stdev_result <- stdev(data)
print(stdev_result)