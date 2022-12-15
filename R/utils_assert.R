assert_jags_file <- function(jags_file) {
  targets::tar_assert_chr(jags_file)
  targets::tar_assert_path(jags_file)
  targets::tar_assert_not_dir(jags_file)
}

assert_transform <- function(transform) {
  if (is.null(transform)) {
    return()
  }
  if (!is.symbol(transform)) {
    targets::tar_throw_validate("transform must be a symbol or NULL.")
  }
  name <- as.character(transform)
  msg <- "transform must be a function in the pipeline environment."
  if (!(name %in% names(targets::tar_option_get("envir")))) {
    targets::tar_throw_validate(msg)
  }
  fun <- targets::tar_option_get("envir")[[name]]
  if (!is.function(fun)) {
    targets::tar_throw_validate(msg)
  }
  args <- names(formals(fun))
  if (!all(c("data", "draws") %in% args)) {
    msg <- "transform must have arguments \"data\" and \"draws\""
    targets::tar_throw_validate(msg)
  }
}
