# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("produce_jags_names()", {
  expect_equal(
    produce_jags_names(c(j = "x/a.jags", k = "x/b.jags")),
    c("j", "k")
  )
  expect_equal(
    produce_jags_names(c("x/a.jags", "x/b.jags")),
    c("a", "b")
  )
  expect_error(
    produce_jags_names(c("x/a.jags", "y/a.jags")),
    class = "condition_validate"
  )
  expect_equal(
    produce_jags_names(c(a = "x/a.jags", b = "y/a.jags")),
    c("a", "b")
  )
  expect_error(
    produce_jags_names(c("x/a.jags", y = "y/asdf.jags")),
    class = "condition_validate"
  )
})
