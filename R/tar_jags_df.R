#' @title Select a strategic piece of `R2jags` output.
#' @export
#' @keywords internal
#' @return A data frame of `R2jags` output. Depends on the `output` argument.
#' @description Not a user-side function. Do not call directly.
#'   Exported for infrastructure reasons only.
#' @param fit `R2jags` object.
#' @param data A list, the original JAGS dataset.
#' @param variables Character vector of model parameter names.
#'   The output posterior summaries are restricted to these variables.
#' @param output Character of length 1 denoting the type of output `tibble`
#'   to return: `"draws"` for MCMC samples (which could take up a lot of space)
#'   `"summary"` for lightweight posterior summary statistics,
#'   and `"dic"` for the overall deviance information criterion
#'   and effective number of parameters.
#' @param summaries List of summary functions passed to `...` in
#'   `posterior::summarize_draws()` through `$summary()`
#'   on the `CmdStanFit` object.
#' @param summary_args List of summary function arguments passed to
#'    `.args` in `posterior::summarize_draws()` through `$summary()`
#'    on the `CmdStanFit` object.
#' @param transform Symbol or `NULL`, name of a function that accepts
#'   arguments `data` and `draws` and returns a data frame. Here,
#'   `data` is the JAGS data list supplied to the model, and `draws`
#'   is a data frame with one column per model parameter and one row
#'   per posterior sample. Any arguments other than `data` and `draws`
#'   must have valid default values because `jagstargets` will not
#'   populate them. See the simulation-based calibration discussion
#'   thread at <https://github.com/ropensci/jagstargets/discussions/31>
#'   for an example.
tar_jags_df <- function(
  fit,
  data,
  output = c("draws", "summary", "dic"),
  variables = NULL,
  summaries = NULL,
  summary_args = NULL,
  transform = NULL
) {
  output <- match.arg(output)
  out <- switch(
    output,
    draws = tar_jags_df_draws(
      fit = fit,
      data = data,
      transform = transform
    ),
    summary = tar_jags_df_summary(
      fit = fit,
      data = data,
      variables = variables,
      summaries = summaries,
      summary_args = summary_args
    ),
    dic = tar_jags_df_dic(fit)
  )
  out$.dataset_id <- data$.dataset_id
  out
}

tar_jags_df_draws <- function(fit, data, transform) {
  list <- fit$BUGSoutput$sims.list
  out <- as.matrix(fit$BUGSoutput$sims.matrix, nrow = nrow(list))
  colnames(out) <- colnames(out) %||% names(list)
  out <- tibble::as_tibble(out)
  chains <- as.integer(fit$BUGSoutput$n.chains)
  iterations <- as.integer(nrow(out) / chains)
  out$.chain <- rep(seq_len(chains), each = iterations)
  out$.iteration <- rep(seq_len(iterations), times = chains)
  out$.draw <- out$.iteration
  if (!is.null(transform)) {
    out <- do.call(
      what = transform,
      args = list(data = data, draws = out),
      envir = targets::tar_option_get("envir")
    )
  }
  out
}

tar_jags_df_summary <- function(
  fit,
  data,
  variables,
  summaries,
  summary_args
) {
  draws <- tar_jags_df_draws(fit = fit, data = data, transform = NULL)
  draws$.draw <- NULL
  if (!is.null(variables)) {
    pattern <- paste(paste0("*", variables, "\\[*"), collapse = "|")
    columns <- grep(pattern, colnames(draws), value = TRUE)
    draws <- draws[, columns, drop = FALSE]
  }
  args <- list(quote(posterior::summarize_draws), .x = quote(draws))
  args$.args <- summary_args
  args <- c(args, summaries)
  out <- eval(as.call(args))
  out <- join_data(out, data$.join_data)
}

join_data <- function(out, data) {
  out$.join_data <- purrr::map_dbl(out$variable, ~join_data_scalar(.x, data))
  out
}

join_data_scalar <- function(text, data) {
  out <- try(eval(parse(text = text), envir = data), silent = TRUE)
  if (!is.vector(out) || length(out) != 1L) {
    out <- NA_real_
  }
  out
}

tar_jags_df_dic <- function(fit) {
  tibble::tibble(
    dic = fit$BUGSoutput$DIC,
    pV = fit$BUGSoutput$pV
  )
}
