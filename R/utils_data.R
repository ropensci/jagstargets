list_nonempty <- function(list) {
  index <- vapply(list, FUN.VALUE = logical(1), FUN = function(x) {
    is.null(x)
  })
  list[!index]
}

produce_seed_rep <- function(name, batch, rep, reps) {
  seed <- if_any(
    "seed" %in% names(formals(targets::tar_option_set)),
    targets::tar_option_get("seed"),
    0L
  )
  if (anyNA(seed)) {
    return(NA_integer_)
  }
  scalar <- paste(name, rep + reps * (batch - 1))
  targets::tar_seed_create(as.character(scalar), global_seed = seed)
}
