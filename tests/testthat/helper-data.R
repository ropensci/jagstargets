test_data <- function() {
  out <- tar_jags_example_data(n = 10)
  out$true_beta <- NULL
  out
}
