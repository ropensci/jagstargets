targets::tar_test("tar_jags_example_file()", {
  tmp <- tempfile()
  expect_false(file.exists(tmp))
  tar_jags_example_file(path = tmp)
  expect_true(file.exists(tmp))
  data <- tar_jags_example_data()
  data$true_beta <- NULL
  msg <- capture.output(
    out <- R2jags::jags(
      data = data,
      parameters.to.save = "beta",
      model.file = tmp,
      n.chains = 3,
      n.iter = 200L,
      n.burn = 100L,
      progress.bar = "none"
    )
  )
  expect_true(inherits(out, "rjags"))
})
