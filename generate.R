generate <- function(conditions, condition_number, rep_set, rep) {
  library(covsim)
  library(EnvStats)
  library(LaplacesDemon)
  #========================================================================
  # seed and variable names
  #========================================================================
  set.seed(100000 * condition_number + 10000 * rep_set + rep)
  n <- as.integer(conditions[condition_number, 1]) # c(100, 250, 500, 1000)
  J <- as.integer(conditions[condition_number, 2]) # c(6, 12)
  norm <- as.integer(conditions[condition_number, 3]) # 1: normal, 2: nonnormal
  K <- as.integer(conditions[condition_number, 4]) # c(2, 5)
  dist <- as.integer(conditions[condition_number, 5]) # 1: quasi-normal, 2: middle, 3: average, 4: floor, 5: flat
  link <- as.integer(conditions[condition_number, 6]) # 1: normal ogive, 2: logistic
  #========================================================================
  # obtain parameters, latent values
  #========================================================================
  params <- getparam(J, norm, K, dist)
  as <- params$as
  bs <- as.matrix(params$bs)
  if (norm == 1) {
    theta <- rnorm(n)
  } else {
    skew <- 3
    kurt <- 21
    # skew <- 3.6
    # kurt <- 15.6
    # skew <- 2
    # kurt <- 7
    theta <- rIG(n, 
                 sigma.target = diag(2), 
                 skewness = rep(skew, 2),
                 excesskurtosis = rep(kurt, 2))[[1]][, 1]
  }
  out <- matrix(vector("double", n * J), n, J)
  #========================================================================
  # IRT
  #========================================================================
  ref <- read_csv("./summary/ref.csv")
  probs <- array(vector("double", n * J * (K - 1)), dim = c(n, J, (K - 1)))
  if (K == 2) {
    among <- c(0, 1)
  } else {
    among <- c(0, 1, 2, 3, 4)
  }
  for (i in 1:n) {
    for (j in 1:J) {
      for (k in 1:(K - 1)) {
        if (link == 1) {
          probs[i, j, k] <- pnorm(as[j] * theta[i] - bs[j, k])
        } else if (link == 2) {
          D <- unlist(ref[condition_number, "D_log"])
          probs[i, j, k] <- invlogit(D * as[j] * theta[i] - D * bs[j, k])
        }
        # } else if (link == 3) {
        #   D <- unlist(ref[condition_number, "D_tri"])
        #   probs[i, j, k] <- ptri(as[j] * theta[i] - bs[j, k], min = -D, max = D)
        # } else {
        #   D <- unlist(ref[condition_number, "D_lap"])
        #   probs[i, j, k] <- plaplace(as[j] * theta[i] - bs[j, k], scale = D)
        # }
      }
      if (K == 2) {
        out[i, j] <- sample(among, size = 1, replace = TRUE,
                            prob = c(1 - probs[i, j, 1], probs[i, j, 1]))
      } else {
        out[i, j] <- sample(among, size = 1, replace = TRUE,
                            prob = c(1 - probs[i, j, 1],
                                     probs[i, j, 1] - probs[i, j, 2],
                                     probs[i, j, 2] - probs[i, j, 3],
                                     probs[i, j, 3] - probs[i, j, 4],
                                     probs[i, j, 4]))
      }
    }
  }
  colnames(out) <- paste0("Item", 1:J)
  return(out)
}
