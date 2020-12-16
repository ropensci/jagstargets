targets::tar_test("tar_jags_example_data()", {
  out <- tar_jags_example_data(n = 50L)
  expect_equal(out$n, 50L)
  expect_equal(length(out$x), 50L)
  expect_equal(length(out$y), 50L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
})
