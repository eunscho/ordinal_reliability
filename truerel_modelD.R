truerel_modelD <- function(as, bs, skew, kurt) {
  rel_no <- irtrel_from_ab(as, bs, skew = skew, kurt = kurt, link = 1)

  from <- 1.6
  to <- 1.9
  
  getDad <- function(Ds, as, bs, skew, kurt, rel_no) {
    ads <- vector("double", length(Ds))
    for (j in 1:length(Ds)) {
      ads[j] <- abs(irtrel_from_ab(as, bs, skew = skew, kurt = kurt, link = 2, D = Ds[j]) - rel_no)
    }
    D <- Ds[which(ads == min(ads))]
    ad <- min(ads)
    list(D = D, ad = ad)
  }
  
  FirstDs <- seq(from = from, to = to, by = .005)
  refD <- getDad(FirstDs, as, bs, skew, kurt, rel_no)$D
  SecondDs <- seq(from = refD - .003, to = refD + .003, by = .001)
  secondout <- getDad(SecondDs, as, bs, skew, kurt, rel_no)
  
  
  D_log <- secondout$D
  ad_log <- secondout$ad
  out <- c(rel_no = rel_no, D_log = D_log, ad_log = ad_log)
  return(out)
}