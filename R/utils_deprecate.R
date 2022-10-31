tar_deprecate_jags_seed <- function(seed) {
  if (is.null(seed)) {
    return()
  }
  targets::tar_warn_deprecate(
    "The jags.seed argument of the tar_jags_rep*() functions is deprecated. ",
    "Rep-specific seeds for the data and models are now automatically set ",
    "based on the batch, rep, parent target name, and ",
    "tar_option_get(\"seed\"). The data seed is in the `.seed` element ",
    "of each data list, and the JAGS seed is in the .seed column ",
    "of the JAGS model output."
  )
}
