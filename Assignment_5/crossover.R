
# Assignment 5 - Crossover Function
# BDA400 - Technical Analysis Using R

crossover <- function(arr1, arr2) {
  
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  crossover_signals <- rep(FALSE, length(arr1))
  
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (!is.na(arr1[i]) && !is.na(arr2[i]) &&
          !is.na(arr1[i - 1]) && !is.na(arr2[i - 1])) {
        crossover_signals[i] <-
          arr1[i] > arr2[i] &&
          arr1[i - 1] <= arr2[i - 1]
      }
    }
  }
  
  return(crossover_signals)
}

# Test Crossover Function
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

crossover_result <- crossover(arr1, arr2)
print(crossover_result)