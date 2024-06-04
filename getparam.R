getparam <- function(J, norm, K, dist) {
  library(tidyverse)
  tau_res <- read_csv("./summary/taus.csv")
  lambdas <- rep(c(.6, .7, .8), J/3)
  tau6 <- tau_res %>% filter(norms == norm &  Ks == K & dists == dist & lambdas == .6) %>% 
                      dplyr::select(t1:t4)
  tau7 <- tau_res %>% filter(norms == norm &  Ks == K & dists == dist & lambdas == .7) %>% 
                      dplyr::select(t1:t4)
  tau8 <- tau_res %>% filter(norms == norm &  Ks == K & dists == dist & lambdas == .8) %>% 
                      dplyr::select(t1:t4)
  if (K == 2) {
    tau6 <- tau6[, 1]
    tau7 <- tau7[, 1]
    tau8 <- tau8[, 1]
  }
  tau6 <- unlist(tau6)
  tau7 <- unlist(tau7)
  tau8 <- unlist(tau8)
  taus <- matrix(rep(c(tau6, tau7, tau8), J/3), byrow = T, ncol = (K-1))
  takane_res <- takane(lambdas, taus)
  as <- takane_res$as
  bs <- takane_res$bs
  
  #########################################################################
  # skewness, kurtosis
  #######################################################################
  # if (K == 2) {
  #   if (dist == 1) {
  #     ratio <- c(.5, .5)
  #   } else if (dist == 2) {
  #     ratio <- c(.6, .4)
  #   } else if (dist == 3) {
  #     ratio <- c(.7, .3)
  #   } else if (dist == 4){
  #     ratio <- c(.8, .2)
  #   } else {
  #     ratio <- c(.9, .1)
  #   }
  # } else {
  #   if (dist == 1) { # quasi-normal
  #     ratio <- c(.05, .21, .48, .21, .05)
  #   } else if (dist == 2) { # middle
  #     ratio <- c(.05, .10, .70, .10, .05)
  #   } else if (dist == 3) { # average
  #     ratio <- c(.25, .35, .23, .11, .06)
  #   } else if (dist == 4) { # floor
  #     ratio <- c(.68, .21, .07, .01, .03)
  #   } else { # flat
  #     ratio <- c(.21, .20, .18, .20, .21)
  #   }
  # }

  #########################################################################
  # as, bs
  #######################################################################
  

  list(as = as, bs = bs)
}
