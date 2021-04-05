produce_jags_names <- function(jags_files) {
  out <- if_any(
    is.null(names(jags_files)),
    tools::file_path_sans_ext(basename(jags_files)),
    names(jags_files)
  )
  assert_unique(out, "target suffixes from jags_files are not unique.")
  assert_nzchar(out, "target suffixes from jags_files must be nonempty.")
  make.names(out)
}
