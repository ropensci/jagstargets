#' targets: Targets for JAGS Workflows
#' @docType package
#' @description The `jagstargets` R package is an extension to
#'   `targets` and `R2jags` for Bayesian data analysis.
#'   `jagstargets` makes it super easy to set up useful scalable
#'   JAGS pipelines that automatically parallelize the computation
#'   and skip expensive steps when the results are already up to date.
#'   Minimal custom code is required, and there is no need to manually
#'   configure branching, so usage is much easier than `targets` alone.
#' @name jagstargets-package
#' @importFrom fst read_fst
#' @importFrom qs qread
#' @importFrom posterior as_draws_df
#' @importFrom purrr map
#' @importFrom R2jags jags jags.parallel
#' @importFrom rlang as_function sym
#' @importFrom stats rnorm
#' @importFrom targets tar_cue tar_dir tar_load tar_option_get tar_path
#'   tar_pipeline tar_read tar_script tar_target tar_target_raw tar_test
#' @importFrom tarchetypes tar_combine tar_map
#' @importFrom tibble as_tibble tibble
#' @importFrom tools file_path_sans_ext
NULL

utils::globalVariables(
  c(
    "._jagstargets_file_50e43091",
    "._jagstargets_name_50e43091",
    "._jagstargets_name_chr_50e43091"
  )
)
