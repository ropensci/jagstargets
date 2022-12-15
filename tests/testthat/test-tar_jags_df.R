# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.

# tar_jags_df() is an internal function to produce clean
# summary output from a JAGS model fit.
# This test checks if the output is formatted correctly.
targets::tar_test("tar_jags_df() with dic", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("R2jags")
  tmp <- tempfile()
  expect_false(file.exists(tmp))
  tar_jags_example_file(path = tmp)
  expect_true(file.exists(tmp))
  data <- tar_jags_example_data(n = 10)
  data$.join_data <- NULL
  dir <- tempfile()
  dir.create(dir)
  msg <- capture.output(
    out <- R2jags::jags(
      data = data,
      parameters.to.save = "beta",
      model.file = tmp,
      n.chains = 3,
      n.iter = 200L,
      n.burn = 100L,
      progress.bar = "none",
      working.directory = dir,
      DIC = TRUE
    )
  )
  out
  # draws
  draws <- tar_jags_df(out, data = data, "draws")
  cols <- c("beta", "deviance", ".chain", ".iteration", ".draw")
  expect_true(all(cols %in% colnames(draws)))
  expect_equal(nrow(draws), 300L)
  # summary
  summary <- tar_jags_df(out, data = data, output = "summary")
  expect_true(
    all(c("variable", "mean", "sd", "q5", ".join_data") %in% colnames(summary))
  )
  expect_true("beta" %in% summary$variable)
  expect_true("deviance" %in% summary$variable)
  expect_true(nrow(summary) < 10L)
  expect_equal(summary$.join_data, rep(NA_real_, 2))
  # summary with joined data and explicit variables
  data$.join_data$beta <- 1
  summary <- tar_jags_df(
    out,
    data = data,
    output = "summary",
    variables = c("beta", "deviance")
  )
  expect_equal(summary$.join_data[summary$variable == "beta"], 1)
  expect_equal(summary$.join_data[summary$variable == "deviance"], NA_real_)
  # custom summary
  summaries <- as.list(
    quote(
      list(
        custom = ~posterior::quantile2(.x, probs = c(0.025, 0.5, 0.975)),
        custom2 = function(x, my_arg) my_arg
      )
    )
  )
  summaries <- summaries[-1]
  summary <- tar_jags_df(
    out,
    data = data,
    output = "summary",
    summaries = summaries,
    summary_args = list(my_arg = 3L)
  )
  expect_true(
    all(c("variable", "q97.5", "q2.5", "custom2") %in% colnames(summary))
  )
  expect_true("beta" %in% summary$variable)
  expect_true("deviance" %in% summary$variable)
  expect_true(all(summary$custom2 == 3L))
  expect_true(nrow(summary) < 10L)
  # dic
  dic <- tar_jags_df(out, data = data, "dic")
  expect_equal(nrow(dic), 1)
  expect_equal(sort(colnames(dic)), sort(c("dic", "pD")))
  # invalid output
  expect_error(tar_jags_df(out, "nope"))
})
