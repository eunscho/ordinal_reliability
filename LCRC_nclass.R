#' Finding the optimal number of class to obtain LCRC
#'
#' Finding the optimal number of class to obtain LCRC. To estimate the LCRC, we
#' need to determine the number of latent classes. Existing criteria for
#' determining are AIC and BIC, but van der Ark et al. (2011) used AIC3.
#' The current version is only available with 6 or 10 items. Requires the poLCA
#' package.
#' @param data a dataframe or matrix
#' @return Optimal number of classes when each criterion (AIC, BIC, AIC3) is applied
#' @references Linzer, D. A., & Lewis, J. B. (2011). PoLCA: An R Package for
#' polytomous variable latent class analysis. Journal of Statistical Software,
#' 42(10). https://doi.org/10.18637/jss.v042.i10
#' @references van der Ark, L. A., van der Palm, D. W., & Sijtsma, K. (2011). A
#' latent class approach to estimating test-score reliability. Applied
#' Psychological Measurement, 35(5), 380-392.
#' https://doi.org/10.1177/0146621610392911
LCRC_nclass <- function(data){
  stopifnot(requireNamespace("poLCA"))
  data <- data.frame(data)
  k <- ncol(data)
  colnames(data) <- LETTERS[1:k]
  if (k == 6) {
    f <- cbind(A, B, C, D, E, F) ~ 1 # van der Ark et al. (2011)
  } else if (k == 12) {
    f <- cbind(A, B, C, D, E, F, G, H, I, J, K, L) ~ 1 # van der Ark et al. (2011)
  } else {
    stop("sorry, the column length should be either six or twelve")
  }
  AIC3 <- AIC <- BIC <-  vector("double", k)
  for (i in 1:k) {
    poLCA_out <- poLCA::poLCA(f, data, nclass = i)
    AIC3[i] <-  -2 * poLCA_out$llik + 3 * poLCA_out$npar # used by van der Ark et al. (2011)
  }
  out <- which.min(AIC3)
  return(out)
}
