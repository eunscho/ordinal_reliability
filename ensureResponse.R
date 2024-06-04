ensureResponse <- function(start_con = 1, end_con = 320){
  n <- c(100, 250, 500, 1000)
  J <- c(6, 12)
  norm <- c(1, 2) # 1: normal, 2: nonnormal (s2, k7)
  K <- c(2, 5) 
  dist <- c(1, 2, 3, 4, 5) # 1: quasi-normal, 2: middle, 3: askew, 4: floor, 5: flat
  link <- c(1, 2) # 1: normal ogive, 2: logistic
  conditions <- tidyr::crossing(n, J, norm, K, dist, link)
  condition_numbers <- start_con:end_con
  name <- "Response_"
  rep_sets <- 1:10
  rep_per_set <- 1:100 
  #========================================================================
  # Loop
  #========================================================================
  for (condition_number in condition_numbers) {
    condition <- conditions[condition_number, ]
    print(condition)
    for (rep_set in rep_sets) {
      print(paste("Starting condition number", condition_number, "rep", rep_set))
      filename <- paste0(name, condition_number, "-", rep_set, ".csv")
      if (!file.exists(filename)) {
        for (rep in rep_per_set) {
          #cat(name, ": ", condition_number, "rep set: ", rep_set, "rep: ", rep)
          n <- as.integer(conditions[condition_number, 1])
          K <- as.integer(conditions[condition_number, 4]) 
          if ((n == 100 & K == 2) |(n < 500 & K == 5)) {
            data <- generate(conditions, condition_number, rep_set, rep)
            J <- as.integer(conditions[condition_number, 2]) 
            if (length(unlist(apply(data, 2, table))) == J * K) {
              atLeastOneResponse <- 1
            } else {
              atLeastOneResponse <- 0
            }
          } else {
            atLeastOneResponse <- 1
          }
          if (rep == 1) {
            out <- tibble::tibble(condition_number, rep_set, rep, atLeastOneResponse)
          } else {
            out <- rbind(out,
                          tibble::tibble(condition_number, rep_set, rep, atLeastOneResponse))
          }
        } # end of for (rep in rep_per_set)
        readr::write_csv(data.frame(out), file = filename)
      } # end of if (!file.exists(filename))
    } # end of  for (rep_set in rep_sets)
  } # end of for (condition_number in condition_numbers)
}