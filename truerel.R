truerel <- function(start = 1, end = 40) {
  J <- c(6, 12)
  norm <- c(1, 2) # 1: normal, 2: nonnormal (s2, k7)
  K <- c(2, 5) 
  dist <- c(1, 2, 3, 4, 5) # 1: quasi-normal, 2: middle, 3: average, 4: floor, 5: flat
  cons <- tidyr::crossing(J, norm, K, dist)
  con_numbers <- start:end
  name <- "truerel_"
  #========================================================================
  # Loop
  #========================================================================
  for (con_number in con_numbers) {
    con <- cons[con_number, ]
    print(paste("Starting con number: ", con_number))
    print(con)
    filename <- paste0(name, con_number, ".csv")
    if (!file.exists(filename)) {
      
      params <- getparam(as.integer(con[1]), as.integer(con[2]), 
                         as.integer(con[3]), as.integer(con[4]))
      as <- params$as
      bs <- params$bs
      if (as.integer(con[2]) == 1) {
        skew <- 0
        kurt <- 0
      } else {
        skew <- 3
        kurt <- 21
        # skew <- 2
        # kurt <- 7
      }
      out <- truerel_modelD(as, bs, skew, kurt)
      readr::write_csv(data.frame(out), file = filename)
    }

  } # end of for (con_number in con_numbers)
}