# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_jags_rep_summary()", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  tar_jags_example_file(path = "a.jags")
  tar_jags_example_file(path = "b.jags")
  targets::tar_script({
    list(
      tar_jags_rep_summary(
        model,
        jags_files = c(x = "a.jags", y = "b.jags"),
       data = tar_jags_example_data(),
        parameters.to.save = "beta",
        variables = "beta",
        log = R.utils::nullfile(),
        refresh = 0,
        n.iter = 2e3,
        n.burnin = 1e3,
        n.thin = 1,
        n.chains = 4,
        batches = 2,
        reps = 2,
        combine = TRUE
      )
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 9L)
  # graph
  out <- targets::tar_network(callr_function = NULL, targets_only = TRUE)$edges
  out <- dplyr::arrange(out, from, to)
  rownames(out) <- NULL
  exp <- tibble::tribble(
    ~from, ~to,
    "model_data", "model_x",
    "model_file_x", "model_lines_x",
    "model_lines_x", "model_x",
    "model_batch", "model_data",
    "model_data", "model_y",
    "model_lines_y", "model_y",
    "model_file_y", "model_lines_y",
    "model_x", "model",
    "model_y", "model"
  )
  exp <- dplyr::arrange(exp, from, to)
  rownames(exp) <- NULL
  expect_equal(out, exp)
  # results
  capture.output(targets::tar_make(callr_function = NULL))
  meta <- tar_meta(starts_with("model_data_"))
  expect_equal(nrow(meta), 2L)
  expect_equal(targets::tar_read(model_file_x), "a.jags")
  expect_equal(targets::tar_read(model_file_y), "b.jags")
  out <- targets::tar_read(model_data)
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_true(is.list(out))
  expect_equal(length(out), 4L)
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  out1 <- targets::tar_read(model_x)
  out2 <- targets::tar_read(model_y)
  out <- targets::tar_read(model)
  expect_true("beta" %in% out$variable)
  expect_true(all(c("mean", "q5") %in% colnames(out)))
  expect_equal(sort(unique(out$.file)), sort(unique(c("a.jags", "b.jags"))))
  expect_equal(sort(unique(out$.name)), sort(unique(c("x", "y"))))
  expect_equal(dplyr::bind_rows(out1, out2), out)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_equal(length(unique(table(out1$.rep))), 1L)
  expect_equal(length(unique(table(out2$.rep))), 1L)
  expect_equal(length(table(out1$.rep)), 4L)
  expect_equal(length(table(out2$.rep)), 4L)
  expect_equal(nrow(out1), 4L)
  expect_equal(nrow(out2), 4L)
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "b.jags", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c("model_file_y", "model_lines_y", "model_y", "model")
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    list(
      tar_jags_rep_summary(
        model,
        jags_files = c(x = "a.jags", y = "b.jags"),
        data = c(tar_jags_example_data()),
        parameters.to.save = "beta",
        variables = "beta",
        log = R.utils::nullfile(),
        refresh = 0,
        n.iter = 2e3,
        n.burnin = 1e3,
        n.thin = 1,
        n.chains = 4,
        batches = 2,
        reps = 2,
        combine = TRUE
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_file_y",
    "model_lines_y",
    "model_y",
    "model",
    "model_x",
    "model_data"
  )
  expect_equal(sort(out), sort(exp))
})

targets::tar_test("tar_jags_rep_summary() with custom summaries", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  tar_jags_example_file(path = "a.jags")
  tar_jags_example_file(path = "b.jags")
  targets::tar_script({
    list(
      tar_jags_rep_summary(
        model,
        jags_files = c(x = "a.jags", y = "b.jags"),
        data = tar_jags_example_data(),
        parameters.to.save = "beta",
        log = R.utils::nullfile(),
        refresh = 0,
        n.iter = 2e3,
        n.burnin = 1e3,
        n.thin = 1,
        n.chains = 4,
        batches = 2,
        reps = 2,
        combine = TRUE,
        summaries = list(
          custom = ~posterior::quantile2(.x, probs = 0.3),
          custom2 = function(x, my_arg) my_arg
        ),
        summary_args = list(my_arg = 34L)
      )
    )
  })
  capture.output(targets::tar_make(callr_function = NULL))
  out <- targets::tar_read(model)
  expect_true("q30" %in% colnames(out))
  expect_true(all(out$custom2 == 34))
})

targets::tar_test("join to summaries", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  path <- system.file(
    "join_data.jags",
    package = "jagstargets",
    mustWork = TRUE
  )
  file.copy(path, "a.jags")
  targets::tar_script({
    sim_data <- function (n = 10L) {
      alpha <- stats::rnorm(n = 1, mean = 0, sd = 1)
      beta <- stats::rnorm(n = n, mean = 0, sd = 1)
      x <- seq(from = -1, to = 1, length.out = n)
      y <- stats::rnorm(n, x * beta, 1)
      .join_data = list(alpha = alpha, beta = beta)
      out <- list(n = n, x = x, y = y, .join_data = .join_data)
      out
    }
    list(
      tar_jags_rep_summary(
        model,
        jags_files = "a.jags",
        data = sim_data(),
        parameters.to.save = c("alpha", "beta"),
        log = R.utils::nullfile(),
        refresh = 0,
        n.iter = 2e3,
        n.burnin = 1e3,
        n.thin = 1,
        n.chains = 4,
        batches = 2,
        reps = 2
      )
    )
  })
  capture.output(targets::tar_make(callr_function = NULL))
  out <- head(targets::tar_read(model, branches = 1), n = 12)
  data <- tar_read(model_data, branches = 1)[[1]][[1]]$.join_data
  expect_equal(out$.join_data[out$variable == "alpha"], data$alpha)
  expect_equal(out$.join_data[grepl("beta", out$variable)], data$beta)
  expect_equal(out$.join_data[out$variable == "deviance"], NA_real_)
})
