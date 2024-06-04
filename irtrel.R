irtrel <- function(data, logistic = T, package = "ltm", estimator = "DWLS") {
  library(psych)
  library(ltm)
  library(mirt)
  J <- ncol(data) # number of items
  K <- max(data) - min(data) + 1 # number of categories
  D <- 1.702
  #########################################################################
  # estimating as(discrimination) and bs(location)
  #######################################################################
  if (logistic == F) {
    q <- ncol(data)
    for(i in 1:q) data[,i] <- ordered(data[,i])
    varnames <- paste0("y", 1:q)
    colnames(data) <- varnames
    loadingName <- paste("a", 1:q, sep = "")
    errorName <- paste("b", 1:q, sep = "")
    model <- "f1 =~ NA*y1 + "
    loadingLine <- paste(paste(loadingName, "*", varnames, sep = ""), collapse = " + ")
    factorLine <- "f1 ~~ 1*f1\n"
    model <- paste(model, loadingLine, "\n", factorLine)
    fit <- cfa(model, data = data, se = "none", ordered = varnames, estimator = estimator)
    if (fit@Fit@converged & lavInspect(fit, "post.check")) {
      lambda <- lavInspect(fit, "est")$lambda
      tau <- lavInspect(fit, "est")$tau
      taus <- matrix(tau, byrow = TRUE, nrow = J)
      asbs <- takane(lambda, taus)
      out <- irtrel_from_ab(asbs$as, asbs$bs, link = 1)
    } else {
      out <- NA
    }
    # safeirt <- purrr::safely(irt.fa)
    # safeirtout <- safeirt(data)
    # if(is.null(safeirtout$error)) {
    #   irtout <- safeirtout$result$irt
    #   as <- as.numeric(irtout$discrimination)
    #   bs <- irtout$difficulty[[1]]
    #   out <- irtrel_from_ab(as, bs, linkage = 1)
    # } else {
    #   out <- NA
    # }
  } else {
    as <- vector("double", J) 
    if (K == 2) {
      bs <- vector("double", J)
    } else {
      bs <- matrix(vector("double", J * (K - 1)), nrow = J)
    }
    if (package == "ltm") {
      if (K == 2) {
        result <- ltm(data ~ z1, IRT.param = TRUE)
        error <- NULL
        irtout <- list(result = result, error = error)
        # safeirt <- purrr::safely(ltm)
        # irtout <- safeirt(data ~ z1, IRT.param = TRUE)
        
      } else {
        safeirt <- purrr::safely(grm)
        irtout <- safeirt(data)
      }
    } else {
      safeirt <- purrr::safely(mirt)
      if (K == 2) {
        irtout <- safeirt(data, itemtype = "2PL")
      } else {
        irtout <- safeirt(data, itemtype = "graded")
      }
    }
    
    if (!is.null(irtout$error)) {
      out <- NA
    } else {
      if (package == "ltm") {
        
        if (K == 2) {
          irtcoef <- coef(irtout$result)
          as <- irtcoef[, 2] / D
          bs <- irtcoef[, 1] / D
          # for (i in 1:J) {
          #   as[i] <- irtcoef[2 + 2 * (i - 1)] / D
          #   bs[i] <- irtcoef[1 + 2 * (i - 1)] / D
          # }
        } else {
          irtcoef <- coef(irtout$result)
          as <- irtcoef[, K] / D
          bs <- irtcoef[, 1:(K - 1)] / D
          # for (i in 1:J) {
          #   as[i] <- irtcoef[K * i] / D
          #   for (k in 1:(K - 1)) {
          #     bs[i, k] <- irtcoef[K * (i - 1) + k] / D
          #   }
          # }
        }
      } else {
        irtcoef <- unlist(coef(irtout$result))
        if (K == 2) {
          for (i in 1:J) {
            as[i] <- irtcoef[1 + 4 * (i - 1)] / D
            bs[i] <- -irtcoef[2 + 4 * (i - 1)] / D
          }
        } else {
          for (i in 1:J) {
            as[i] <- irtcoef[K * (i - 1) + 1] / D
            for (k in 1:(K - 1)) {
              bs[i, k] <- -irtcoef[K * (i - 1) + 1 + k] / D
            }
          }
        }
      }
      out <- irtrel_from_ab(as, bs, link = 2, D = D)
    } # end of  } else {
  } # end of   } else {
  #########################################################################
  # obtaining reliability
  #######################################################################
  return(out)
}

