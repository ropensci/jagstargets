#' @title Write an example JAGS model file.
#' @export
#' @description Overwrites the file at `path` with a built-in example
#'   JAGS model file.
#' @return `NULL` (invisibly).
#' @param path Character of length 1, file path to write the model file.
#' @examples
#' path <- tempfile()
#' tar_jags_example_file(path = path)
#' writeLines(readLines(path))
tar_jags_example_file <- function(path = "jagstargets_example.jags") {
  src <- system.file("example.jags", package = "jagstargets", mustWork = TRUE)
  file.copy(src, path, overwrite = TRUE)
  invisible()
}
