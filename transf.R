transf <- function(value, threshold) {
  for (i in 1:length(threshold)) {
    if (value > threshold[length(threshold) - i + 1]) {
      out <- length(threshold) - i + 1
      break
    } else {
      out <- 0
    }
  }
  return(out)
}

