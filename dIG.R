dIG <- function(x, sigma = 1, skewness = 0, excesskurtosis = 0) {
  # This function is a modification of the code written by Professor Foldnes
  library(PearsonDS)
  out <- vector("double",length(x))
  for (i in 1:length(x)) {
    IG_params <- function(sigma, skewness, excesskurtosis, typeA=c("symm", "triang") ){
      sigma.target <-as.matrix(sigma)
      nvar          <- dim(sigma.target)[2]
      #define functions
      function.skew <- function(IGvalues){
        fval <- numeric(nvar)
        for (i in 1:nvar)
          fval[i] <-   A[i, ]^3 %*% IGvalues/(sum(A[i,]^2)^(3/2))
        
        fval-skewness
      }
      function.kurt <- function(IGvalues){
        fval <- numeric(nvar)
        for (i in 1:nvar)
          fval[i] <-   A[i, ]^4 %*% IGvalues/(sum(A[i,]^2)^(2))
        
        fval-excesskurtosis
      }
      #calculate A
      typeA <- match.arg(typeA)
      if(typeA=="triang")
        A  <- t(chol(sigma.target))
      else
        A  <- lavaan::lav_matrix_symmetric_sqrt(sigma.target)#symmetric
      IGskew        <- nleqslv::nleqslv(x=skewness, function.skew)$x
      IGkurt.excess <- nleqslv::nleqslv(x=excesskurtosis, function.kurt)$x
      parlist       <- list()
      for (i in 1:nvar) parlist[[i]] <- PearsonDS::pearsonFitM(moments=c(mean=0, variance=1, skewness=IGskew[i], 3+IGkurt.excess[i]))
      
      return(list(parlist=parlist, A=A, Ainv=solve(A), Adet = det(A)))
    }
    
    speclist <- IG_params(sigma, skewness, excesskurtosis)
    if (is.vector(x[i]))
      x[i] <- matrix(x[i], nrow = length(x[i]))
    A  <- speclist$A
    Ainv <- speclist$Ainv
    Adet <- speclist$Adet
    parlist <- speclist$parlist
    
    xtransformed <- Ainv %*% x[i]
    
    # the univariate densities
    uni <- lapply(1:length(parlist), function(dim){
      dpearson(xtransformed[dim,], params=parlist[[dim]])
    })
    uni <- do.call(rbind,uni)
    if (is.vector(uni))
      uni <- matrix(uni, nrow = length(uni))
    uniprod <- apply(uni, 2, prod)
    out[i] <- matrix(uniprod/Adet, nrow=1)[[1]]
  }
  return(out)

}
