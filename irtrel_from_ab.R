irtrel_from_ab <- function(as, bs, skew = 0, kurt  = 0, link = 2, D = 1.702) {
  # library(EnvStats)
  # library(LaplacesDemon)
  if (is.matrix(bs)) {
    J <- nrow(bs) 
    K <- ncol(bs) + 1
  } else {
    J <- length(bs)
    K <- 2
  }
  #########################################################################
  # basic functions
  #######################################################################
  logistic_f <- function(x) {
    1 / (1 + exp(-x))
  }
    
  cumprob <- function(x, as, bs, link, j, k) {
    if (k == 0) {
      out <- 1
    } else {
      if (link == 1) {
        if (K == 2) {
          out <- pnorm(as[j] * x - bs[j])
        } else {
          out <- pnorm(as[j] * x - bs[j, k])
        }
      } else if (link == 2) {
        if (K == 2) {
          out <- logistic_f(D * (as[j] * x - bs[j]))
        } else {
          out <- logistic_f(D * (as[j] * x - bs[j, k]))
        }
      # } else if (link == 3) {
      #   if (K == 2) {
      #     out <- ptri(as[j] * x - bs[j], min = -D, max = D, mode = 0)
      #   } else {
      #     out <- ptri(as[j] * x - bs[j, k], min = -D, max = D, mode = 0)
      #   }
      # } else if (link == 4) {
      #   if (K == 2) {
      #     out <- plaplace(as[j] * x - bs[j], scale = D)
      #   } else {
      #     out <- plaplace(as[j] * x - bs[j, k], scale = D)
      #   }
      # }
      }
    }
    return(out)
  }

  prob <- function(x, as, bs, link, j, k, K) {
    if (k == K - 1) {
      out <- cumprob(x, as, bs, link, j, k)
    } else {
      out <- cumprob(x, as, bs, link, j, k) - cumprob(x, as, bs, link, j, k + 1)
    }
    return(out)
  }
  #########################################################################
  # functions to obtain reliability, formula 2 & 5
  #######################################################################
  # var[X|theta], formula 2
  fun_f2 <- function(x, as, bs, link, K) {
    # formula 3
    e_x_theta <-  0
    for (j in 1:J) {
      for (k in 1:(K - 1)) {
        e_x_theta <- e_x_theta + k * prob(x, as, bs, link, j, k, K)
      }
    }
    # formula 4
    fun_f4 <- function(x, as, bs,link, K) {
      sum <- 0
      for (j in 1:J) {
        for (k in 1:(K - 1)) {
          if (skew == 0 & kurt == 0) {
            sum <- sum + k * prob(x, as, bs, link, j, k, K) * dnorm(x)
          } else {
            sum <- sum + k * prob(x, as, bs, link, j, k, K) * dIG(x, skewness = skew, excesskurtosis = kurt)
          }
        }
      }
      sum
    }
    out_f4 <- integrate(fun_f4, -Inf, Inf, as = as, bs = bs, link = link, K = K)$value
    # formula2
    if (skew == 0 & kurt == 0) {
      out <- (e_x_theta - out_f4)^2 * dnorm(x)
    } else {
      out <- (e_x_theta - out_f4)^2 * dIG(x, skewness = skew, excesskurtosis = kurt)
    }
    
    return(out)
  }  
  # E[var(X|theta)], formula 5
  fun_f5 <- function(x, as, bs, link, K) {
    sum <- 0
    for (j in 1:J) {
      e_xj_theta <- 0 # E[Xj|theta], formula 7
      for (k in 1:(K - 1)) {
        e_xj_theta <- e_xj_theta + k * prob(x, as, bs, link, j, k, K)
      }
      var_xj_theta <- 0 # var(Xj|theta), formula 6
      for (k in 0:(K - 1)) {
        var_xj_theta <- var_xj_theta + prob(x, as, bs, link, j, k, K) * (k - e_xj_theta)^2
      }
      sum <- sum + var_xj_theta
    }
    if (skew == 0 & kurt == 0) {
      out <- sum * dnorm(x)
    } else {
      out <- sum * dIG(x, skewness = skew, excesskurtosis = kurt)
    }
    return(out)
  }
  #########################################################################
  #  obtain reliability
  #######################################################################
  out_f2 <- integrate(fun_f2, -Inf, Inf, as = as, bs = bs, link = link, K = K)$value
  out_f5 <- integrate(fun_f5, -Inf, Inf, as = as, bs = bs, link = link, K = K)$value
  out <- out_f2 / (out_f2 + out_f5)
  return(out)
}
  