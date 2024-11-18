skips <- function() {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("qs")
  skip_if_not_installed("qs2")
  skip_if_not_installed("rjags")
  skip_if_not_installed("R2jags")
}
