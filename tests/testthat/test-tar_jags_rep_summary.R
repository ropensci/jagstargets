# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.

# tar_jags_rep_summary() creates a JAGS pipeline that
# runs multiple MCMCs on multiple JAGS models
# and returns data frames of MCMC summaries in the output. This test
# checks that the pipeline is correctly constructed
# and the output is correctly formatted.
targets::tar_test("tar_jags_rep_summary()", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rjags")
  skip_if_not_installed("R2jags")
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
  # data
  capture.output(targets::tar_make(callr_function = NULL))
  meta <- tar_meta(starts_with("model_data_"))
  expect_equal(nrow(meta), 2L)
  expect_equal(targets::tar_read(model_file_x), "a.jags")
  expect_equal(targets::tar_read(model_file_y), "b.jags")
  out <- targets::tar_read(model_data)
  dataset_ids <- c(
    out[[1]][[1]]$.dataset_id,
    out[[1]][[2]]$.dataset_id,
    out[[2]][[1]]$.dataset_id,
    out[[2]][[2]]$.dataset_id
  )
  expect_equal(length(unique(dataset_ids)), 4)
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_true(is.list(out))
  expect_equal(length(out), 6L)
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  # model
  out1 <- targets::tar_read(model_x)
  out2 <- targets::tar_read(model_y)
  out <- targets::tar_read(model)
  expect_equal(unique(table(out$.dataset_id)), 2)
  expect_equal(length(unique(out$.dataset_id)), 4)
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
  # Change the model. Some targets should invalidate.
  write("", file = "b.jags", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c("model_file_y", "model_lines_y", "model_y", "model")
  expect_equal(sort(out), sort(exp))
  # Change the data code. Some targets should invalidate.
  targets::tar_script({
    list(
      tar_jags_rep_summary(
        model,
        jags_files = c(x = "a.jags", y = "b.jags"),
        data = c(tar_jags_example_data()),
        parameters.to.save = "beta",
        variables = "beta",
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

targets::tar_test("tar_jags_rep_summary() with custom summaries", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rjags")
  skip_if_not_installed("R2jags")
  tar_jags_example_file(path = "a.jags")
  tar_jags_example_file(path = "b.jags")
  targets::tar_script({
    list(
      tar_jags_rep_summary(
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
  skip_if_not_installed("rjags")
  skip_if_not_installed("R2jags")
  path <- system.file(
    "join_data.jags",
    package = "jagstargets",
    mustWork = TRUE
  )
  file.copy(path, "a.jags")
  targets::tar_script({
    sim_data <- function(n = 10L) {
      alpha <- stats::rnorm(n = 1, mean = 0, sd = 1)
      beta <- stats::rnorm(n = n, mean = 0, sd = 1)
      x <- seq(from = -1, to = 1, length.out = n)
      y <- stats::rnorm(n, x * beta, 1)
      .join_data <- list(alpha = alpha, beta = beta)
      out <- list(n = n, x = x, y = y, .join_data = .join_data)
      out
    }
    list(
      tar_jags_rep_summary(
        model,
        jags_files = "a.jags",
        data = sim_data(),
        parameters.to.save = c("alpha", "beta"),
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile(),
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

targets::tar_test("tar_jags_rep_summary() errors if no JAGS file", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("R2jags")
  expect_error(
    tar_jags_rep_summary(
      model,
      jags_files = "a.jags",
      data = sim_data(),
      parameters.to.save = c("alpha", "beta"),
      stdout = R.utils::nullfile(),
      stderr = R.utils::nullfile(),
      refresh = 0,
      n.iter = 2e3,
      n.burnin = 1e3,
      n.thin = 1,
      n.chains = 4,
      batches = 2,
      reps = 2
    ),
    class = "tar_condition_validate"
  )
})

targets::tar_test("tar_jags_rep_summary() seed resilience", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rjags")
  skip_if_not_installed("R2jags")
  tar_jags_example_file(path = "a.jags")
  targets::tar_script({
    list(
      tar_jags_rep_summary(
        model,
        jags_files = c(x = "a.jags"),
        data = tar_jags_example_data(),
        parameters.to.save = "beta",
        variables = "beta",
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
  targets::tar_make(callr_function = NULL)
  data1 <- tar_read(model_data)
  expect_equal(length(data1), 2L)
  model1 <- tar_read(model_x)
  targets::tar_script({
    list(
      tar_jags_rep_summary(
        model,
        jags_files = c(x = "a.jags"),
        data = tar_jags_example_data(),
        parameters.to.save = "beta",
        variables = "beta",
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile(),
        refresh = 0,
        n.iter = 2e3,
        n.burnin = 1e3,
        n.thin = 1,
        n.chains = 4,
        batches = 1,
        reps = 4,
        combine = TRUE
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  data2 <- tar_read(model_data)
  expect_equal(length(data2), 1L)
  model2 <- tar_read(model_x)
  data_list1 <- list(
    data1[[1]][[1]],
    data1[[1]][[2]],
    data1[[2]][[1]],
    data1[[2]][[2]]
  )
  for (index in seq_len(4)) {
    data_list1[[index]]$.dataset_id <- NULL
    data2[[1]][[index]]$.dataset_id <- NULL
  }
  expect_equal(data_list1, data2[[1]])
  for (field in c(".dataset_id", ".rep")) {
    model1[[field]] <- NULL
    model2[[field]] <- NULL
  }
  expect_equal(model1, model2)
})
