assert_jags_file <- function(jags_file) {
  targets::tar_assert_chr(jags_file)
  targets::tar_assert_path(jags_file)
  if (dir.exists(jags_file)) {
    throw_validate("jags_file must not be a directory.")
  }
}
