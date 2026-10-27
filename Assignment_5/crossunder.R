
# Assignment 5 - Crossunder Function
# BDA400 - Technical Analysis Using R

crossunder <- function(arr1, arr2) {
  
  if (length(arr1) != length(arr2)) {
    stop("Both arrays must have the same length")
  }
  
  crossunder_signals <- rep(FALSE, length(arr1))
  
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (!is.na(arr1[i]) && !is.na(arr2[i]) &&
          !is.na(arr1[i - 1]) && !is.na(arr2[i - 1])) {
        
        crossunder_signals[i] <-
          arr1[i] < arr2[i] &&
          arr1[i - 1] >= arr2[i - 1]
      }
    }
  }
  
  return(crossunder_signals)
}

# Test Crossunder Function
arr1 <- c(20, 22, 25, 24, 21, 15, 12, 10, 8)
arr2 <- c(10, 12, 15, 18, 20, 18, 15, 12, 11)

crossunder_result <- crossunder(arr1, arr2)
print(crossunder_result)