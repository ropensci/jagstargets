targets::tar_test("tar_deprecate_jags_seed()", {
  expect_silent(tar_deprecate_jags_seed(NULL))
  expect_warning(
    tar_deprecate_jags_seed(0L),
    class = "tar_condition_deprecate"
  )
})
