#' @title Example data for [tar_jags_example_file()]
#' @export
#' @description An example dataset compatible with the model file
#'   from [tar_jags_example_file()]. The output has a `.join_data`
#'   element so the true value of `beta` from the simulation
#'   is automatically appended to the `beta` rows of the
#'   summary output.
#' @return List, dataset compatible with the model file from
#'   [tar_jags_example_file()]. The output has a `.join_data`
#'   element so the true value of `beta` from the simulation
#'   is automatically appended to the `beta` rows of the
#'   summary output.
#' @param n Integer of length 1, number of data points.
#' @examples
#' tar_jags_example_data()
tar_jags_example_data <- function(n = 10L) {
  beta <- stats::rnorm(n = 1, mean = 0, sd = 1)
  x <- seq(from = -1, to = 1, length.out = n)
  y <- stats::rnorm(n, x * beta, 1)
  out <- list(
    n = n,
    x = x,
    y = y,
    .join_data = list(beta = beta)
  )
  out
}
