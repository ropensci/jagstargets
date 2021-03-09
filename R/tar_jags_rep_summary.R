#' @title Tidy posterior summaries from multiple MCMCs per model.
#' @export
#' @description Run multiple MCMCs on simulated datasets
#'   and return posterior summaries and the effective number of parameters
#'   for each run.
#' @details The MCMC targets use `R2jags::jags()` if `n.cluster` is `1` and
#'   `R2jags::jags.parallel()` otherwise. Most arguments to `tar_jags()`
#'   are forwarded to these functions.
#' @return `tar_jags_rep_dic(name = x, jags_files = "y.jags")`
#'   returns a list of `targets::tar_target()` objects:
#'   * `x_file_y`: reproducibly track the jags model file.
#'   * `x_lines_y`: contents of the jags model file.
#'   * `x_data`: dynamic branching target with simulated datasets.
#'   * `x_y`: dynamic branching target with tidy data frames of posterior
#'     summaries.
#'   * `x`: combine all the model-specific summary targets into
#'     a single data frame with columns to distinguish among the models.
#'     Suppressed if `combine` is `FALSE`.
#'
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
#' @inheritParams tar_jags_rep
#' @examples
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#' library(jagstargets)
#' # Do not use a temp file for a real project
#' # or else your targets will always rerun.
#' tmp <- tempfile()
#' tar_jags_example_file(tmp)
#' list(
#'   tar_jags_rep_summary(
#'     your_model,
#'     jags_files = tmp,
#'     data = tar_jags_example_data(),
#'     parameters.to.save = "beta",
#'     batches = 2,
#'     reps = 2,
#'     log = R.utils::nullfile()
#'   )
#' )
#' }, ask = FALSE)
#' targets::tar_make()
#' })
tar_jags_rep_summary <- function(
  name,
  jags_files,
  parameters.to.save,
  data = list(),
  variables = NULL,
  summaries = NULL,
  summary_args = NULL,
  batches = 1L,
  reps = 1L,
  combine = TRUE,
  n.cluster = 1,
  n.chains = 3,
  n.iter = 2e3,
  n.burnin = as.integer(n.iter / 2),
  n.thin = 1,
  jags.module = c("glm", "dic"),
  inits = NULL,
  RNGname = c(
    "Wichmann-Hill",
    "Marsaglia-Multicarry",
    "Super-Duper",
    "Mersenne-Twister"
  ),
  jags.seed = 123,
  log = NULL,
  progress.bar = "text",
  refresh = 0,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  error = targets::tar_option_get("error"),
  memory = "transient",
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  tar_jags_rep(
    name = deparse_language(substitute(name)),
    jags_files = jags_files,
    parameters.to.save = parameters.to.save,
    data = substitute(data),
    variables = variables,
    summaries = substitute(summaries),
    summary_args = substitute(summary_args),
    batches = batches,
    reps = reps,
    output = "summary",
    combine = combine,
    n.cluster = n.cluster,
    n.chains = n.chains,
    n.iter = n.iter,
    n.burnin = n.burnin,
    n.thin = n.thin,
    jags.module = jags.module,
    inits = inits,
    RNGname = RNGname,
    jags.seed = jags.seed,
    log = log,
    progress.bar = progress.bar,
    refresh = refresh,
    tidy_eval = tidy_eval,
    packages = packages,
    library = library,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}
