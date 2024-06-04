truerel_fa <- function(start = 1, end = 40) {
  library(covsim)
  J <- c(6, 12)
  norm <- c(1, 2) # 1: normal, 2: nonnormal (s2, k7)
  K <- c(2, 5) 
  dist <- c(1, 2, 3, 4, 5) # 1: quasi-normal, 2: middle, 3: average, 4: floor, 5: flat
  cons <- tidyr::crossing(J, norm, K, dist)
  con_numbers <- start:end
  n <- 10^7
  rep <- 10
  name <- "truerelFA_"
  #========================================================================
  # Loop
  #========================================================================
  for (con_number in con_numbers) {
    con <- cons[con_number, ]
    print(paste("Starting con number: ", con_number))
    print(con)
    filename <- paste0(name, con_number, ".csv")
    if (!file.exists(filename)) {
      J <- unlist(con[1])
      norm <- unlist(con[2])
      K <- unlist(con[3])
      dist <- unlist(con[4])
      params <- getparam(J, norm, K, dist)
      params_fa <- takane_opposite(params$as, params$bs)
      loadings <- params_fa$loadings
      taus <- params_fa$taus
      rel <- vector("double", rep)
      for (l in 1:rep) {
        set.seed(l)
        #theta <- rnorm(n)
        if (norm == 1) {
          skew <- 0
          kurt <- 0
        } else {
          skew <- 3
          kurt <- 21
        }
        theta <- rIG(n, diag(2), rep(skew, 2), rep(kurt, 2))[[1]][,1]
        e <- matrix(rnorm(n * J), nrow = n, ncol = J)
        parallel_e <- matrix(rnorm(n * J), nrow = n, ncol = J)
        score_con <- theta %*% t(loadings) + e %*% sqrt(diag(J) - diag(diag(loadings %*% t(loadings))))
        parallel_con <-theta %*% t(loadings) + parallel_e %*% sqrt(diag(J) - diag(diag(loadings %*% t(loadings))))
        score_ord <- matrix(vector("double", n * J), nrow = n)
        parallel_ord <- matrix(vector("double", n * J), nrow = n)
        for (i in 1:n) {
          for (j in 1:J) {
            score_ord[i, j] <- transf(score_con[i, j], taus[j, ])
            parallel_ord[i, j] <- transf(parallel_con[i, j], taus[j, ])
          }
        }
        rel[l] <- sum(cov(score_ord, parallel_ord)) / sqrt(sum(var(score_ord)) * sum(var(parallel_ord)))
      }
      out <- c(con_number = con_number, rel = rel)
      readr::write_csv(data.frame(out), file = filename)
    }
  } # end of for (con_number in con_numbers)
}