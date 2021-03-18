#' @title Tidy output from multiple MCMCs per model.
#' @keywords internal
#' @description Internal function. Users should not invoke directly.
#' @return `tar_jags_rep(name = x, jags_files = "y.jags")`
#'   returns a list of `targets::tar_target()` objects:
#'   * `x_file_y`: reproducibly track the jags model file.
#'   * `x_lines_y`: contents of the jags model file.
#'   * `x_data`: dynamic branching target with simulated datasets.
#'   * `x_y`: dynamic branching target with tidy data frames of MCMC summaries.
#'   * `x`: combine all the model-specific summary targets into
#'     a single data frame with columns to distinguish among the models.
#'     Suppressed if `combine` is `FALSE`.
#' @inheritParams tar_jags
#' @inheritParams tar_jags_rep_run
#' @param batches Number of batches. Each batch runs a model `reps` times.
#' @param reps Number of replications per batch. Ideally, each rep
#'   should produce its own random dataset using the code
#'   supplied to `data`.
#' @param combine Logical, whether to create a target to
#'   combine all the model results
#'   into a single data frame downstream. Convenient, but
#'   duplicates data.
tar_jags_rep <- function(
  name,
  jags_files,
  parameters.to.save,
  data = quote(list()),
  batches = 1L,
  reps = 1L,
  output = c("summary", "draws", "dic"),
  variables = NULL,
  summaries = NULL,
  summary_args = NULL,
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
  jags.seed = 1,
  stdout = NULL,
  stderr = NULL,
  progress.bar = "text",
  refresh = 0,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  envir <- tar_option_get("envir")
  assert_chr(jags_files, "jags_files must be a character vector")
  assert_unique(jags_files, "jags_files must be unique")
  output <- match.arg(output)
  name_jags <- produce_jags_names(jags_files)
  name_file <- paste0(name, "_file")
  name_lines <- paste0(name, "_lines")
  name_batch <- paste0(name, "_batch")
  name_data <- paste0(name, "_data")
  sym_jags <- rlang::syms(name_jags)
  sym_file <- rlang::sym(name_file)
  sym_lines <- rlang::sym(name_lines)
  sym_batch <- rlang::sym(name_batch)
  sym_data <- rlang::sym(name_data)
  command_lines <- call_function(
    "readLines",
    args = list(con = rlang::sym(name_file))
  )
  command_batch <- substitute(seq_len(x), env = list(x = batches))
  command_rep <- tidy_eval(data, envir = envir, tidy_eval = tidy_eval)
  command_data <- substitute(
    purrr::map(seq_len(.targets_reps), ~.targets_command),
    env = list(.targets_reps = reps, .targets_command = command_rep)
  )
  args <- list(
    call_ns("jagstargets", "tar_jags_rep_run"),
    jags_lines = sym_lines,
    jags_name = quote(._jagstargets_name_chr_50e43091),
    jags_file = quote(._jagstargets_file_50e43091),
    parameters.to.save = parameters.to.save,
    data = sym_data,
    variables = variables,
    summaries = summaries,
    summary_args = summary_args,
    reps = reps,
    output = output,
    n.cluster = n.cluster,
    n.chains = n.chains,
    n.iter = n.iter,
    n.burnin = n.burnin,
    n.thin = n.thin,
    jags.module = jags.module,
    inits = inits,
    RNGname = RNGname,
    jags.seed = jags.seed,
    stdout = stdout,
    stderr = stderr,
    progress.bar = progress.bar,
    refresh = refresh
  )
  command <- as.expression(as.call(args))
  pattern_data <- substitute(map(x), env = list(x = sym_batch))
  pattern <- substitute(map(x), env = list(x = sym_data))
  target_file <- targets::tar_target_raw(
    name = name_file,
    command = quote(._jagstargets_file_50e43091),
    packages = character(0),
    format = "file",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    cue = cue
  )
  target_lines <- targets::tar_target_raw(
    name = name_lines,
    command = command_lines,
    packages = character(0),
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    cue = cue
  )
  target_batch <- targets::tar_target_raw(
    name = name_batch,
    command = command_batch,
    packages = character(0),
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    cue = cue
  )
  target_data <- targets::tar_target_raw(
    name = name_data,
    command = command_data,
    pattern = pattern_data,
    packages = packages,
    library = library,
    format = "qs",
    iteration = "list",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    cue = cue
  )
  target_mcmc <- targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = character(0),
    format = "fst_tbl",
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
  out <- list(target_file, target_lines, target_mcmc)
  out <- list_nonempty(out)
  values <- list(
    ._jagstargets_file_50e43091 = jags_files,
    ._jagstargets_name_50e43091 = sym_jags,
    ._jagstargets_name_chr_50e43091 = name_jags
  )
  out <- tarchetypes::tar_map(
    values = values,
    names = ._jagstargets_name_50e43091,
    unlist = TRUE,
    out
  )
  out[[name_data]] <- target_data
  out[[name_batch]] <- target_batch
  names_mcmc <- paste0(name, "_", name_jags)
  if (combine) {
    out[[name]] <- tarchetypes::tar_combine_raw(
      name = name,
      out[names_mcmc],
      packages = character(0),
      format = "fst_tbl",
      iteration = "vector",
      error = error,
      memory = memory,
      garbage_collection = garbage_collection,
      deployment = "main",
      priority = priority,
      resources = resources,
      storage = "main",
      retrieval = "main",
      cue = cue
    )
  }
  out
}

#' @title Run a batch of iterations for a jags model
#'   and return only strategic output.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A data frame of posterior summaries.
#' @inheritParams tar_jags_df
#' @inheritParams tar_jags_run
#' @param jags_lines Character vector of lines from a JAGS file.
#' @param jags_name Friendly suffix of the jags model target.
#' @param jags_file Original path to the input jags file.
tar_jags_rep_run <- function(
  jags_lines,
  jags_name,
  jags_file,
  parameters.to.save,
  data,
  variables = NULL,
  summaries = NULL,
  summary_args = NULL,
  reps,
  output,
  n.cluster = n.cluster,
  n.chains = n.chains,
  n.iter = n.iter,
  n.burnin = n.burnin,
  n.thin = n.thin,
  jags.module = jags.module,
  inits = inits,
  RNGname = RNGname,
  jags.seed = jags.seed,
  stdout = stdout,
  stderr = stderr,
  progress.bar = progress.bar,
  refresh = refresh
) {
  out <- purrr::map_dfr(
    .x = data,
    ~tar_jags_rep_run_rep(
      data = .x,
      jags_lines = jags_lines,
      jags_name = jags_name,
      jags_file = jags_file,
      parameters.to.save = parameters.to.save,
      variables = variables,
      summaries = summaries,
      summary_args = summary_args,
      reps = reps,
      output = output,
      n.cluster = n.cluster,
      n.chains = n.chains,
      n.iter = n.iter,
      n.burnin = n.burnin,
      n.thin = n.thin,
      jags.module = jags.module,
      inits = inits,
      RNGname = RNGname,
      jags.seed = jags.seed,
      stdout = stdout,
      stderr = stderr,
      progress.bar = progress.bar,
      refresh = refresh
    )
  )
  out$.file <- jags_file
  out$.name <- jags_name
  out
}

tar_jags_rep_run_rep <- function(
  jags_lines,
  jags_name,
  jags_file,
  parameters.to.save,
  data,
  variables,
  summaries,
  summary_args,
  reps,
  output,
  n.cluster,
  n.chains,
  n.iter,
  n.burnin,
  n.thin,
  jags.module,
  inits,
  RNGname,
  jags.seed,
  stdout,
  stderr,
  progress.bar,
  refresh
) {
  jags_data <- data
  jags_data$.join_data <- NULL
  fit <- tar_jags_run(
    jags_lines = jags_lines,
    parameters.to.save = parameters.to.save,
    data = jags_data,
    inits = inits,
    n.cluster = n.cluster,
    n.chains = n.chains,
    n.iter = n.iter,
    n.burnin = n.burnin,
    n.thin = n.thin,
    jags.module = jags.module,
    RNGname = RNGname,
    jags.seed = jags.seed,
    stdout = stdout,
    stderr = stderr,
    progress.bar = progress.bar,
    refresh = refresh
  )
  out <- tar_jags_df(
    fit = fit,
    data = data,
    output = output,
    variables = variables,
    summaries = summaries,
    summary_args = summary_args
  )
  out$.rep <- digest::digest(runif(1), algo = "xxhash32")
  out
}
