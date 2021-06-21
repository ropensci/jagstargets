# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.

# These tests check internal assertion routines
# used to verify the correctness of user inputs.
targets::tar_test("assert_jags_file()", {
  tmp <- tempfile()
  file.create(tmp)
  expect_silent(assert_jags_file(tmp))
  expect_error(
    assert_jags_file("nopenopenope"),
    class = "tar_condition_validate"
  )
})

targets::tar_test("assert_jags_file() on a directory", {
  tmp <- tempfile()
  dir.create(tmp)
  expect_error(
    assert_jags_file(tmp),
    class = "tar_condition_validate"
  )
})
