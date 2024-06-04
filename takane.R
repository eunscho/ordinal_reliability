takane <- function(loadings, taus) {
  as <- loadings / sqrt(1 - loadings^2)
  if (is.matrix(taus)) {
    J <- nrow(taus)
    K <- ncol(taus) + 1
  } else {
    J <- length(loadings)
    K <- 2
  }
  bs <- matrix(vector("double", J * (K - 1)), nrow = J)
  for (j in 1:J) {
    for (k in 1:(K - 1)) {
      bs[j, k] <- taus[j, k] / sqrt(1 - loadings[j]^2)
    }
  }
  return(list(as = as, bs = bs))
}