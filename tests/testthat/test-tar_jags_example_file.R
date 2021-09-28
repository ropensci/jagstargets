# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_jags_example_file() creates a valid JAGS model file.", {
  tmp <- tempfile()
  expect_false(file.exists(tmp))
  tar_jags_example_file(path = tmp)
  expect_true(file.exists(tmp))
  data <- tar_jags_example_data(n = 10)
  data$.join_data <- NULL
  dir <- tempfile()
  dir.create(dir)
  skip_if_not_installed("rjags")
  skip_if_not_installed("R2jags")
  msg <- capture.output(
    out <- R2jags::jags(
      data = data,
      parameters.to.save = "beta",
      model.file = tmp,
      n.chains = 3,
      n.iter = 200L,
      n.burn = 100L,
      progress.bar = "none",
      working.directory = dir
    )
  )
  expect_true(inherits(out, "rjags"))
})
