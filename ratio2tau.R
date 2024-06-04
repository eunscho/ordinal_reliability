ratio2tau <- function(ratio, skew = 0, kurt = 0, lambda = .7) {
  if (skew == 0 & kurt == 0) {
    out <- qnorm(cumsum(ratio))[1:(length(ratio) - 1)]
  } else {
    library(covsim)
    n <- 10^8
    rep <- 1:10
    out_rep <- matrix(vector("double", length(rep) * (length(ratio) - 1)), nrow = length(rep))
    for (i in rep) {
      set.seed(i)
      x <- lambda * rIG(n, 
                        sigma.target = diag(2), 
                        skewness = rep(skew,2), 
                        excesskurtosis = rep(kurt, 2))[[1]][, 1] +
        rnorm(n, sd = sqrt(1 - lambda^2))
      out_rep[i, ] <- sort(x)[cumsum(ratio) * n][1:(length(ratio) - 1)]
      out <- apply(out_rep, 2, mean)
    }
  }
  out
}
