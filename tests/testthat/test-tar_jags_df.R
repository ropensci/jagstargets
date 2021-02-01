# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_jags_df() with dic", {
  tmp <- tempfile()
  expect_false(file.exists(tmp))
  tar_jags_example_file(path = tmp)
  expect_true(file.exists(tmp))
  data <- tar_jags_example_data(n = 10)
  data$true_beta <- NULL
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
  draws <- tar_jags_df(out, "draws")
  cols <- c("beta", "deviance", ".chain", ".iteration", ".draw")
  expect_true(all(cols %in% colnames(draws)))
  expect_equal(nrow(draws), 300L)
  # summary
  summary <- tar_jags_df(out, "summary")
  expect_true(all(c("variable", "mean", "sd", "q5") %in% colnames(summary)))
  expect_true("beta" %in% summary$variable)
  expect_true("deviance" %in% summary$variable)
  expect_true(nrow(summary) < 10L)
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
  dic <- tar_jags_df(out, "dic")
  expect_equal(nrow(dic), 1)
  expect_equal(sort(colnames(dic)), sort(c("dic", "pD")))
  # invalid output
  expect_error(tar_jags_df(out, "nope"))
})
