#' @title Tidy output from multiple MCMCs per model.
#' @keywords internal
#' @description Internal function. Users should not invoke directly.
#' @section Seeds:
#'   Rep-specific random number generator seeds for the data and models
#'   are automatically set based on the batch, rep,
#'   parent target name, and `tar_option_get("seed")`. This ensures
#'   the rep-specific seeds do not change when you change the batching
#'   configuration (e.g. 40 batches of 10 reps each vs 20 batches of 20
#'   reps each). Each data seed is in the `.seed` list element of the output,
#'   and each JAGS seed is in the .seed column of each JAGS model output.
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
#' @param jags.seed The `jags.seed` argument of the `tar_jags_rep*()`
#'   functions is deprecated. See the "Seeds" section for details.
#' @param batches Number of batches. Each batch runs a model `reps` times.
#' @param reps Number of replications per batch. Ideally, each rep
#'   should produce its own random dataset using the code
#'   supplied to `data`.
#' @param combine Logical, whether to create a target to
#'   combine all the model results
#'   into a single data frame downstream. Convenient, but
#'   duplicates data.
#' @param format Character of length 1, storage format of the data frames
#'   of posterior summaries and other data frames returned by targets.
#'   We recommend efficient data frame formats
#'   such as `"feather"` or `"aws_parquet"`. For more on storage formats,
#'   see the help file of `targets::tar_target()`.
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
  transform = NULL,
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
  jags.seed = NULL,
  stdout = NULL,
  stderr = NULL,
  progress.bar = "text",
  refresh = 0,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = "qs",
  format_df = "fst_tbl",
  repository = targets::tar_option_get("repository"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  targets::tar_assert_package("rjags")
  targets::tar_assert_package("R2jags")
  tar_deprecate_jags_seed(jags.seed)
  envir <- tar_option_get("envir")
  targets::tar_assert_chr(jags_files, "jags_files must be a character vector")
  targets::tar_assert_unique(jags_files, "jags_files must be unique")
  lapply(jags_files, assert_jags_file)
  output <- match.arg(output)
  assert_transform(transform)
  name_jags <- produce_jags_names(jags_files)
  name_file <- paste0(name, "_file")
  name_lines <- paste0(name, "_lines")
  name_batch <- paste0(name, "_batch")
  name_data <- paste0(name, "_data")
  sym_jags <- as_symbols(name_jags)
  sym_file <- as.symbol(name_file)
  sym_lines <- as.symbol(name_lines)
  sym_batch <- as.symbol(name_batch)
  sym_data <- as.symbol(name_data)
  command_lines <- call_function(
    "readLines",
    args = list(con = as.symbol(name_file))
  )
  command_batch <- substitute(seq_len(x), env = list(x = batches))
  command_rep <- targets::tar_tidy_eval(
    data,
    envir = envir,
    tidy_eval = tidy_eval
  )
  command_data <- substitute(
    jagstargets::tar_jags_rep_data_batch(
      .targets_reps,
      .targets_batch,
      .targets_command
    ),
    env = list(
      .targets_reps = reps,
      .targets_batch = sym_batch,
      .targets_command = command_rep
    )
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
    transform = transform,
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
    repository = "local",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    cue = cue,
    description = description
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
    cue = cue,
    description = description
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
    cue = cue,
    description = description
  )
  target_data <- targets::tar_target_raw(
    name = name_data,
    command = command_data,
    pattern = pattern_data,
    packages = packages,
    library = library,
    format = format,
    repository = repository,
    iteration = "list",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    cue = cue,
    description = description
  )
  target_mcmc <- targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = character(0),
    format = format_df,
    repository = repository,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    description = description
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
    names = tidyselect::any_of("._jagstargets_name_50e43091"),
    descriptions = tidyselect::any_of("._jagstargets_file_50e43091"),
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
      format = format_df,
      repository = repository,
      iteration = "vector",
      error = error,
      memory = memory,
      garbage_collection = garbage_collection,
      deployment = "main",
      priority = priority,
      resources = resources,
      storage = "main",
      retrieval = "main",
      cue = cue,
      description = description
    )
  }
  out
}

#' @title Generate a batch of data
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A list of JAGS datasets containing data and dataset IDs.
#' @param reps Positive integer of length 1, number of reps to run.
#' @param batch Positive integer of length 1, index of the current batch.
#' @param command R code to run to generate one dataset.
#' @examples
#' tar_jags_rep_data_batch(2, 1, tar_jags_example_data())
tar_jags_rep_data_batch <- function(reps, batch, command) {
  envir <- parent.frame()
  command <- substitute(command)
  purrr::map(
    seq_len(reps),
    ~tar_jags_rep_data_rep(.x, reps, batch, command, envir)
  )
}

tar_jags_rep_data_rep <- function(rep, reps, batch, command, envir) {
  name <- targets::tar_definition()$pedigree$parent
  seed <- produce_seed_rep(name = name, batch = batch, rep = rep, reps = reps)
  out <- if_any(
    anyNA(seed),
    eval(command, envir = envir),
    withr::with_seed(
      seed = seed,
      code = eval(command, envir = envir)
    )
  )
  out$.dataset_id <- paste0(targets::tar_name(), "_", rep)
  out$.seed <- as.integer(seed)
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
  transform = NULL,
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
  stdout = stdout,
  stderr = stderr,
  progress.bar = progress.bar,
  refresh = refresh
) {
  targets::tar_assert_package("rjags")
  targets::tar_assert_package("R2jags")
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
      transform = transform,
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
  transform,
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
  stdout,
  stderr,
  progress.bar,
  refresh
) {
  seed <- data$.seed + 1L
  jags_data <- data
  jags_data$.dataset_id <- NULL
  jags_data$.join_data <- NULL
  jags_data$.seed <- NULL
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
    jags.seed = seed,
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
    summary_args = summary_args,
    transform = transform
  )
  out$.rep <- secretbase::siphash13(runif(1))
  out$.seed <- seed
  out
}
