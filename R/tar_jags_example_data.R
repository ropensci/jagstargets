#' @title Example data for [tar_jags_example_file()]
#' @export
#' @description An example dataset compatible with the model file
#'   from [tar_jags_example_file()].
#' @return List, dataset compatible with the model file from
#'   [tar_jags_example_file()].
#' @param n Integer of length 1, number of data points.
#' @param true_params Logical, whether to include the true values of
#'   the parameters used to simulate the data.
#' @examples
#' tar_jags_example_data()
tar_jags_example_data <- function(n = 10L, true_params = TRUE) {
  true_beta <- stats::rnorm(n = 1, mean = 0, sd = 1)
  x <- seq(from = -1, to = 1, length.out = n)
  y <- stats::rnorm(n, x * true_beta, 1)
  out <- list(
    n = n,
    x = x,
    y = y
  )
  if (true_params) {
    out$true_beta <- true_beta
  }
  out
}
