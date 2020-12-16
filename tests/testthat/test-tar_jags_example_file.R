targets::tar_test("tar_jags_example_file()", {
  tmp <- tempfile()
  expect_false(file.exists(tmp))
  tar_jags_example_file(path = tmp)
  expect_true(file.exists(tmp))
  data <- tar_jags_example_data()
  data$true_beta <- NULL
  model <- rjags::jags.model(tmp, data = data, n.chains = 3, quiet = TRUE)
  out <- rjags::coda.samples(
    model,
    variable.names = "beta",
    n.iter = 100,
    n.chains = 3L,
    progress.bar = "none"
  )
  expect_true(inherits(out, "mcmc.list"))
  expect_equal(length(out), 3L)
  expect_equal(dim(out[[1]]), c(100L, 1L))
})
