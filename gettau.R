gettau <- function() {
  norm <- c(1, 2)
  Ks <- c(2, 5) 
  dists <- c(1, 2, 3, 4, 5) # 1: quasi-normal, 2: middle, 3: average, 4: floor, 5: flat
  lambdas <- c(.6, .7, .8) 
  cons <- tidyr::crossing(norm, Ks, dists, lambdas)
  out <- matrix(vector("double", nrow(cons) * 8), ncol = 8)
  colnames(out) <- c("norm", "K", "dist", "lambda", "t1", "t2", "t3", "t4")
  for (i in 1:nrow(cons)) {
    out[i, 1] <-  norm <- as.integer(cons[i, 1])
    out[i, 2] <- K <- as.integer(cons[i, 2])
    out[i, 3] <- dist <- as.integer(cons[i, 3])
    out[i, 4] <- lambda <- as.double(cons[i, 4])
    if (norm == 1) {
      skew <- 0
      kurt <- 0
    } else {
      skew <- 3
      kurt <- 21
      # skew <- 2
      # kurt <- 7
    }
    if (K == 2) {
      if (dist == 1) {
        ratio <- c(.5, .5)
      } else if (dist == 2) {
        ratio <- c(.6, .4)
      } else if (dist == 3) {
        ratio <- c(.7, .3)
      } else if (dist == 4){
        ratio <- c(.8, .2)
      } else {
        ratio <- c(.9, .1)
      }
    } else {
      if (dist == 1) { # quasi-normal
        ratio <- c(.05, .21, .48, .21, .05)
      } else if (dist == 2) { # middle
        ratio <- c(.05, .10, .70, .10, .05)
      } else if (dist == 3) { # average
        ratio <- c(.25, .35, .23, .11, .06)
      } else if (dist == 4) { # floor
        ratio <- c(.55, .25, .10, .05, .05)
        #ratio <- c(.68, .21, .07, .01, .03)
      } else { # flat
        ratio <- c(.21, .20, .18, .20, .21)
      }
    }
    tau <- ratio2tau(ratio, skew, kurt, lambda)
    if (K == 2) {
      out[i, 5] <- tau
      out[i, 6:8] <- NA
    } else {
      out[i, 5:8] <- tau
    }
  }
  write.csv(out, "taus.csv")
  out
}
