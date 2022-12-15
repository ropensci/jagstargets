#' jagstargets: Targets for JAGS Workflows
#' @docType package
#' @description Bayesian data analysis usually incurs long runtimes
#'   and cumbersome custom code. A pipeline toolkit tailored to
#'   Bayesian statisticians, the `jagstargets` R package leverages
#'   `targets` and `R2jags` to ease this burden.
#'   `jagstargets` makes it super easy to set up scalable
#'   JAGS pipelines that automatically parallelize the computation
#'   and skip expensive steps when the results are already up to date.
#'   Minimal custom code is required, and there is no need to manually
#'   configure branching, so usage is much easier than `targets` alone.
#' @name jagstargets-package
#' @seealso <https://docs.ropensci.org/jagstargets/>, [tar_jags()]
#' @aliases jagstargets
#' @importFrom coda mcmc mcmc.list
#' @importFrom digest digest digest2int
#' @importFrom fst read_fst
#' @importFrom posterior summarize_draws
#' @importFrom purrr map map_dbl map_dfr
#' @importFrom qs qread
#' @importFrom rlang expr quo_squash
#' @importFrom stats rnorm runif
#' @importFrom targets tar_assert_chr tar_assert_in
#'   tar_assert_nonempty tar_assert_not_dir
#'   tar_assert_not_in tar_assert_nzchar
#'   tar_assert_path tar_assert_scalar tar_assert_unique
#'   tar_definition tar_deparse_language tar_deparse_safe
#'   tar_cue tar_dir tar_load tar_name tar_option_get tar_option_set
#'   tar_read tar_script tar_target tar_target_raw tar_test
#'   tar_throw_validate tar_tidy_eval tar_warn_deprecate
#' @importFrom tarchetypes tar_combine tar_map
#' @importFrom tibble as_tibble tibble
#' @importFrom tools file_path_sans_ext
#' @importFrom utils capture.output head
#' @importFrom withr local_dir local_message_sink local_output_sink
#'   local_seed
NULL

utils::globalVariables(
  c(
    "._jagstargets_file_50e43091",
    "._jagstargets_name_50e43091",
    "._jagstargets_name_chr_50e43091"
  )
)
