assert_jags_file <- function(jags_file) {
  targets::tar_assert_chr(jags_file)
  targets::tar_assert_path(jags_file)
  targets::tar_assert_not_dir(jags_file)
}
