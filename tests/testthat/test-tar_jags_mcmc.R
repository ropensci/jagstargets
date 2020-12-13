tar_test("tar_jags_mcmc()", {
  library(rjags)
  expect_silent(tar_jags_mcmc())
})
