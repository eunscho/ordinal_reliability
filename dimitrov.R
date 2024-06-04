dimitrov <- function(data) {
  library(ltm)
  irt.mtf <- ltm(data ~ z1, IRT.param = TRUE)
  Param   <-coef(irt.mtf)
  b2PL <- Param[,1]
  # if (Rasch) {
  #
  # }
  a2PL <- Param[,2]/1.702 # scaling constant necessary here

  out <- DQ(a2PL, b2PL)
  return(out)
}
