takane_opposite <- function(as, bs) {
  loadings <- as / sqrt(1 + as^2)
  if (is.matrix(bs)) {
    J <- nrow(bs) 
    K <- ncol(bs) + 1
  } else {
    J <- length(bs)
    K <- 2
    bs <- as.matrix(bs)
  }
  taus <- matrix(vector("double", J * (K - 1)), nrow = J)
  for (j in 1:J) {
    for (k in 1:(K - 1)) {
      taus[j, k] <- bs[j, k] / sqrt(1 + as[j]^2)
    }
  }
  return(list(loadings = loadings, taus = taus))
}