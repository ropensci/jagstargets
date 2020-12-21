#' @title Select a strategic piece of `R2jags` output.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not call directly.
#'   Exported for infrastructure reasons only.
#' @param fit `R2jags` object.
#' @param output Character of length 1 denoting the type of output `tibble`
#'   to return: `"draws"` for MCMC samples (which could take up a lot of space)
#'   `"summary"` for lightweight posterior summary statistics,
#'   and `"dic"` for the overall deviance information criterion
#'   and effective number of parameters
tar_jags_df <- function(
  fit,
  output = c("draws", "summary", "dic")
) {
  out <- match.arg(output)
  switch(
    output,
    draws = tar_jags_df_draws(fit),
    summary = tar_jags_df_summary(fit),
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

tar_jags_df_summary <- function(fit) {
  out <- fit$BUGSoutput$summary
  out <- cbind(variable = rownames(out), out)
  tibble::as_tibble(out, .name_repair = make.names)
}

tar_jags_df_dic <- function(fit) {
  tibble::tibble(
    dic = fit$BUGSoutput$DIC,
    pD = fit$BUGSoutput$pD
  )
}
