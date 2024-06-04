sim2024_2 <- function(start_con = 1, end_con = 320, start_rep = 1, end_rep = 10) {
  library(tidyverse)
  library(psych)
  library(tictoc)
  library(LaplacesDemon)
  # specify simulation conditions
  n <- c(100, 250, 500, 1000)
  J <- c(6, 12)
  norm <- c(1, 2) # 1: normal, 2: nonnormal (s2, k7)
  K <- c(2, 5) 
  dist <- c(1, 2, 3, 4, 5) # 1: quasi-normal, 2: middle, 3: askew, 4: floor, 5: flat
  link <- c(1, 2) # 1: normal ogive, 2: logistic
  conditions <- tidyr::crossing(n, J, norm, K, dist, link)
  condition_numbers <- start_con:end_con
  name <- "sim2024_"
  rep_sets <- start_rep:end_rep
  rep_per_set <- 1:100 
  #========================================================================
  # Loop
  #========================================================================
  for (condition_number in condition_numbers) {
    condition <- conditions[condition_number, ]
    print(condition)
    for (rep_set in rep_sets) {
      tictoc::tic()
      print(paste("Starting condition number", condition_number, "rep", rep_set))
      print(condition)
      filename <- paste0(name, condition_number, "-", rep_set, ".csv")

      if (!file.exists(filename)) {
        for (rep in rep_per_set) {
          cat(name, ": ", condition_number, "rep set: ", rep_set, "rep: ", rep)
          data <- generate(conditions, condition_number, rep_set, rep)
          if (rep == 1) {
            temp <- analyze(conditions, condition_number, rep_set, rep, data)
          } else {
            temp <- rbind(temp,
                          analyze(conditions, condition_number, rep_set, rep, data))
          }
        } # end of for (rep in rep_per_set)
        out <- temp
        readr::write_csv(data.frame(out), file = filename)
        print(out)
        tictoc::toc()
      } # end of if (!file.exists(filename))
    } # end of  for (rep_set in rep_sets)
  } # end of for (condition_number in condition_numbers)
}
