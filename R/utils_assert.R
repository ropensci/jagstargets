assert_chr <- function(x, msg = NULL) {
  if (!is.character(x)) {
    throw_validate(msg %|||% "x must be a character.")
  }
}

assert_in <- function(x, choices, msg = NULL) {
  if (!all(x %in% choices)) {
    throw_validate(msg %|||% paste(deparse(x), "is in", deparse(choices)))
  }
}

assert_nonempty <- function(x, msg = NULL) {
  if (!length(x)) {
    throw_validate(msg %|||% "x must not be empty")
  }
}

assert_not_in <- function(x, choices, msg = NULL) {
  if (any(x %in% choices)) {
    throw_validate(msg %|||% paste(deparse(x), "is in", deparse(choices)))
  }
}

assert_nzchar <- function(x, msg = NULL) {
  if (any(!nzchar(x))) {
    throw_validate(msg %|||% "x has empty character strings")
  }
}

assert_path <- function(path, msg = NULL) {
  assert_nonempty(path, "path must not be empty")
  missing <- !file.exists(path)
  if (any(missing)) {
    throw_validate(
      msg %|||% paste(
        "missing files: ",
        paste(path[missing], collapse = ", ")
      )
    )
  }
}

assert_scalar <- function(x, msg = NULL) {
  if (length(x) != 1) {
    throw_validate(msg %|||% "x must have length 1.")
  }
}

assert_jags_file <- function(jags_file) {
  assert_scalar(jags_file, "only one jags model file allowed at a time.")
  assert_chr(jags_file, "jags_file must be a character.")
  assert_path(jags_file, "jags_file is not an existing file.")
  if (dir.exists(jags_file)) {
    throw_validate("jags_file must not be a directory.")
  }
}

assert_unique <- function(x, msg = NULL) {
  if (anyDuplicated(x)) {
    dups <- paste(unique(x[duplicated(x)]), collapse = ", ")
    throw_validate(paste(msg %|||% "duplicated entries:", dups))
  }
}
