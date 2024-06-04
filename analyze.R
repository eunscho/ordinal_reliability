analyze <- function(conditions, condition_number, rep_set, rep, data) {
  library(mokken)
  library(lavaan)
  library(reliacoef)
  set.seed(100000 * condition_number + 10000 * rep_set + rep)
  n <- as.integer(conditions[condition_number, 1]) # c(100, 250, 500, 1000)
  J <- as.integer(conditions[condition_number, 2]) # c(6, 12)
  norm <- as.integer(conditions[condition_number, 3]) # 1: normal, 2: nonnormal
  K <- as.integer(conditions[condition_number, 4]) # c(2, 5)
  dist <- as.integer(conditions[condition_number, 5]) # 1: quasi-normal, 2: middle, 3: average, 4: floor, 5: flat
  link <- as.integer(conditions[condition_number, 6]) # 1: normal ogive, 2: logistic
  m <- var(data)
  r <- cov2cor(m)
  #===========================================================================
  # reliability
  #===========================================================================
  alpha <- reliacoef::alpha(m, print = F)
  mu2 <- mu2(m, print = F)
  safekaiser <- purrr::safely(kaisercaffrey)
  kaiser_out <- safekaiser(m)$result
  if (is.null(kaiser_out)) {
    kaiser <- NA
  } else {
    kaiser <- kaiser_out
  }
  #kaiser <- kaisercaffrey(m)
  joreskog <- joreskog(m, print = F)
  # ordinal alpha
  safepolychoric <- purrr::safely(lavCor)
  polycor <- safepolychoric(data, ordered = TRUE)$result
  # safepolychoric <- purrr::safely(psych::polychoric)
  # polycor <- safepolychoric(data)$result
  if (is.null(polycor)) {
    ord_alpha <- NA
  } else {
    ord_alpha <- alpha(polycor, print = F)
  }
  if (K == 2) {
    dimitrov <- dimitrov(data)
  } else{
    dimitrov <-  NA
  }
  safe_irtrel <- purrr::safely(irtrel)
  rho_no_dwls_out <- safe_irtrel(data, logistic = F, estimator = "DWLS")$result
  if (is.null(rho_no_dwls_out)) {
    rho_no_dwls <- NA
  } else {
    rho_no_dwls <- rho_no_dwls_out
  }
  rho_no_uls_out <- safe_irtrel(data, logistic = F, estimator = "ULS")$result
  if (is.null(rho_no_uls_out)) {
    rho_no_uls <- NA
  } else {
    rho_no_uls <- rho_no_uls_out
  }
    # rho_no_dwls <- irtrel(data, logistic = F, estimator = "DWLS")
  # rho_no_uls <- irtrel(data, logistic = F, estimator = "ULS")
  rho_lo_ltm_out <- safe_irtrel(data, logistic = T, package = "ltm")$result
  if (is.null(rho_lo_ltm_out)) {
    rho_lo_ltm <- NA
  } else {
    rho_lo_ltm <- rho_lo_ltm_out
  }
  rho_lo_mirt_out <- safe_irtrel(data, logistic = T, package = "mirt")$result
  if (is.null(rho_lo_mirt_out)) {
    rho_lo_mirt <- NA
  } else {
    rho_lo_mirt <- rho_lo_mirt_out
  }
  # rho_lo_ltm <- irtrel(data, logistic = T, package = "ltm")
  # rho_lo_mirt <- irtrel(data, logistic = T, package = "mirt")
  safe_catOmega <- purrr::safely(catOmega)
  catomega_dwls_out <- safe_catOmega(data, "DWLS")$result
  if (is.null(catomega_dwls_out)) {
    catomega_dwls <- NA
  } else {
    catomega_dwls <- catomega_dwls_out
  }
  catomega_uls_out <- safe_catOmega(data, "ULS")$result
  if (is.null(catomega_uls_out)) {
    catomega_uls <- NA
  } else {
    catomega_uls <- catomega_uls_out
  }
  # catomega_dwls <- catOmega(data, "DWLS")
  # catomega_uls <- catOmega(data, "ULS")
  # catomega_misty <- misty::item.omega(data, type = "categ")$result$omega$omega
  # catomega_MBESS <- MBESS::ci.reliability(data, type = "categorical", interval.type = "none")$est
  # catomega_bf <- greenyang2009(data)

  data <- data + 1 # to make all values positive integers
  safenclass <- purrr::safely(LCRC_nclass)
  nclass <- safenclass(data) # the poLCA package requires positive integer data
  if (is.null(nclass$error)) {
    nclass <- nclass$result
    safe_mokken <- safely(mokken::check.reliability)
    safe_mokken_out <- safe_mokken(data, LCRC = TRUE, nclass = nclass)$result
    if(is.null(safe_mokken_out)) {
      MS <- LCRC_AIC3 <- NA
    } else {
      MS <- safe_mokken_out$MS
      LCRC_AIC3 <- safe_mokken_out$LCRC
    }
    #   
    # 
    # mokken_AIC3 <- mokken::check.reliability(data, LCRC = TRUE, nclass = nclass)
    # MS <- mokken_AIC3$MS
    # LCRC_AIC3 <- mokken_AIC3$LCRC
  } else {
    MS <- LCRC_AIC3 <- NA
  }
  out <- tibble::tibble(condition_number, 
                        #n, J, K, dist, model, disc, loc,
                        rep_set, rep, alpha, mu2, joreskog, kaiser,
                        ord_alpha, dimitrov, rho_no_dwls, rho_no_uls,
                        rho_lo_ltm, rho_lo_mirt, catomega_dwls,catomega_uls,MS, LCRC_AIC3)
  return(out)
}
