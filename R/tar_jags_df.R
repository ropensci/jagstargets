#' @title Select a strategic piece of `R2jags` output.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not call directly.
#'   Exported for infrastructure reasons only.
#' @inheritParams posterior::summarize_draws
#' @param fit `R2jags` object.
#' @param output Character of length 1 denoting the type of output `tibble`
#'   to return: `"draws"` for MCMC samples (which could take up a lot of space)
#'   `"summary"` for lightweight posterior summary statistics,
#'   and `"dic"` for the overall deviance information criterion
#'   and effective number of parameters
#' @param summaries List of summary functions passed to `...` in
#'   `posterior::summarize_draws()` through `$summary()`
#'   on the `CmdStanFit` object.
#' @param summary_args List of summary function arguments passed to
#'    `.args` in `posterior::summarize_draws()` through `$summary()`
#'    on the `CmdStanFit` object.
tar_jags_df <- function(
  fit,
  output = c("draws", "summary", "dic"),
  variables = NULL,
  summaries = NULL,
  summary_args = NULL
) {
  out <- match.arg(output)
  switch(
    output,
    draws = tar_jags_df_draws(fit),
    summary = tar_jags_df_summary(fit, variables, summaries, summary_args),
    dic = tar_jags_df_dic(fit)
  )
}

tar_jags_df_draws <- function(fit) {
  list <- fit$BUGSoutput$sims.list
  out <- as.matrix(fit$BUGSoutput$sims.matrix, nrow = nrow(list))
  colnames(out) <- names(list)
  out <- tibble::as_tibble(out, .name_repair = make.names)
  chains <- as.integer(fit$BUGSoutput$n.chains)
  iterations <- as.integer(nrow(out) / chains)
  out$.chain <- rep(seq_len(chains), each = iterations)
  out$.iteration <- rep(seq_len(iterations), times = chains)
  out$.draw <- out$.iteration
  out
}

tar_jags_df_summary <- function(fit, variables, summaries, summary_args) {
  draws <- tar_jags_df_draws(fit)
  draws$.draw <- NULL
  if (!is.null(variables)) {
    draws <- draws[, intersect(variables, colnames(draws)), drop = FALSE]
  }
  args <- list(quote(posterior::summarize_draws), x = quote(draws))
  args$.args <- summary_args
  args <- c(args, summaries)
  eval(as.call(args))
}

tar_jags_df_dic <- function(fit) {
  tibble::tibble(
    dic = fit$BUGSoutput$DIC,
    pD = fit$BUGSoutput$pD
  )
}
