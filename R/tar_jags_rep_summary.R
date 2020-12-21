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
#' @inheritParams tar_jags_rep
#' @examples
#' # First, write your JAGS model file. Example:
#' # tar_jags_example_file() # Writes jagstargets_example.jags
#' # Then in _targets.R, write the pipeline:
#' targets::tar_pipeline(
#'   tar_jags_rep_summary(
#'     your_model,
#'     jags_files = "jagstargets_example.jags",
#'     data = tar_jags_example_data(),
#'     parameters.to.save = "beta",
#'     batches = 40,
#'     reps = 25
#'   )
#' )
tar_jags_rep_summary <- function(
  name,
  jags_files,
  parameters.to.save,
  data = list(),
  data_copy = character(0),
  data_omit = character(0),
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
  quiet = TRUE,
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
    data_copy = data_copy,
    data_omit = data_omit,
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
    quiet = quiet,
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
