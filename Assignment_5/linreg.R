# Assignment 5 - Linear Regression
# BDA400 - Technical Analysis Using R

linreg <- function(regressionSource, regressionLength,
                   regressionOffset) {
  
  n <- length(regressionSource)
  
  if (regressionLength > n || regressionLength < 2) {
    stop("regressionLength must be between 2 and data length")
  }
  
  if (regressionOffset < 0 ||
      regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  
  start_index <- max(1, n - regressionLength +
                       regressionOffset)
  end_index <- min(n, n - regressionOffset)
  
  source_subset <- regressionSource[start_index:end_index]
  index_values <- seq_along(source_subset)
  
  sum_index <- sum(index_values)
  sum_source <- sum(source_subset)
  
  mean_index <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)
  
  numerator <- sum((index_values - mean_index) *
                     (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  
  if (denominator == 0) {
    stop("At least two regression points are required")
  }
  
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  
  predicted_values <- slope * index_values + intercept
  
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}

# Test Linear Regression
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

linreg_result <- linreg(
  regressionSource = data,
  regressionLength = 5,
  regressionOffset = 0
)

print(linreg_result)