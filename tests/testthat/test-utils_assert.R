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

targets::tar_test("assert_transform()", {
  targets::tar_option_set(envir = new.env(parent = emptyenv()))
  envir <- targets::tar_option_get("envir")
  envir[["good1"]] <- function(data, draws) draws
  envir[["good2"]] <- function(draws, data, x) draws
  envir[["bad1"]] <- 123
  envir[["bad2"]] <- function(dat, draws) draws
  expect_silent(assert_transform(NULL))
  expect_silent(assert_transform(quote(good1)))
  expect_silent(assert_transform(quote(good2)))
  class <- "tar_condition_validate"
  expect_error(assert_transform(123), class = class)
  expect_error(assert_transform(quote(nothing)), class = class)
  expect_error(assert_transform(quote(bad1)), class = class)
  expect_error(assert_transform(quote(bad2)), class = class)
})
