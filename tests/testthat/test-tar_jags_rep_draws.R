# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.

# tar_jags_rep_draws() creates a JAGS pipeline that
# runs multiple MCMCs on multiple JAGS models
# and returns data frames of MCMC draws in the output. This test
# checks that the pipeline is correctly constructed
# and the output is correctly formatted.
targets::tar_test("tar_jags_rep_draws()", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  tar_jags_example_file(path = "a.jags")
  tar_jags_example_file(path = "b.jags")
  targets::tar_script({
    list(
      tar_jags_rep_draws(
        model,
        jags_files = c(x = "a.jags", y = "b.jags"),
        data = tar_jags_example_data(),
        parameters.to.save = "beta",
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile(),
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
  # Enough targets are created.
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 9L)
  # Nodes in the graph are connected properly.
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
  # The pipeline produces correctly formatted output.
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
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
  expect_true("beta" %in% colnames(out))
  expect_equal(sort(unique(out$.file)), sort(unique(c("a.jags", "b.jags"))))
  expect_equal(sort(unique(out$.name)), sort(unique(c("x", "y"))))
  expect_equal(dplyr::bind_rows(out1, out2), out)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_equal(length(unique(table(out1$.rep))), 1L)
  expect_equal(length(unique(table(out2$.rep))), 1L)
  expect_equal(length(table(out1$.rep)), 4L)
  expect_equal(length(table(out2$.rep)), 4L)
  expect_equal(nrow(out1), 16000L)
  expect_equal(nrow(out2), 16000L)
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model. Some targets should invalidate.
  write("", file = "b.jags", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c("model_file_y", "model_lines_y", "model_y", "model")
  expect_equal(sort(out), sort(exp))
  # Change the data code. Some targets should invalidate.
  targets::tar_script({
    list(
      tar_jags_rep_draws(
        model,
        jags_files = c(x = "a.jags", y = "b.jags"),
        data = c(tar_jags_example_data()),
        parameters.to.save = "beta",
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile(),
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

targets::tar_test("tar_jags_rep_draws() correctly errors if no JAGS file", {
  expect_error(
    tar_jags_rep_draws(
      model,
      jags_files = c(x = "a.jags", y = "b.jags"),
      data = c(tar_jags_example_data()),
      parameters.to.save = "beta",
      stdout = R.utils::nullfile(),
      stderr = R.utils::nullfile(),
      refresh = 0,
      n.iter = 2e3,
      n.burnin = 1e3,
      n.thin = 1,
      n.chains = 4,
      batches = 2,
      reps = 2,
      combine = TRUE
    ),
    class = "tar_condition_validate"
  )
})
