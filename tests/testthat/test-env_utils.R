library(jute)
context("Env utils")

# ============================================================================ #
# Tests
# ============================================================================ #

# ---------------------------------------------------------------------------- #
test_that("place_in_env writes variables to supplied env", {
  an_env <- new.env(parent = emptyenv())
  place_in_env(x = 1, y = 2, envir = an_env)

  is_found <- exists("x", envir = environment(), inherits = FALSE)
  expect_false(is_found, "info: !exists('x')")

  msg <- "object 'y' not found"
  expect_error(y + 1, regexp = msg, info = "info: y + 1 # => y not found")

  expect_true(exists("y", envir = an_env), "info: exists('y', envir = an_env)")

  expect_equal(an_env$x, 1, info = "info: an_env$x")
  expect_equal(an_env$y, 2, info = "info: an_env$y")
})

# ---------------------------------------------------------------------------- #
test_that("place_in_env with default envir has no effect", {
  # this will place the variables in the environment local to place_in_env
  place_in_env(x = 1, y = 2)

  is_found <- exists("x", envir = environment(), inherits = FALSE)
  expect_false(is_found, "info: !exists('x')")

  is_found <- exists("y", envir = environment(), inherits = FALSE)
  expect_false(is_found, "info: !exists('y')")
})

# ---------------------------------------------------------------------------- #
test_that("place_in_env sets explicit names", {
  an_env <- new.env(parent = emptyenv())
  place_in_env(x = 1, y = 2, envir = an_env, var_names = c("one", "two"))

  info <- "info: !exists('x', envir = an_env)"
  expect_false(exists("x", envir = an_env), info)

  info <- "info: exists('two', envir = an_env)"
  expect_true(exists("two", envir = an_env), info)
})

# ---------------------------------------------------------------------------- #
test_that("place_in_env sets default names", {
  an_env <- new.env(parent = emptyenv())
  place_in_env("apple", "pear", envir = an_env)

  expect_equal(an_env$a, "apple", info = "info: an_env$a")
  expect_equal(an_env$b, "pear",  info = "info: an_env$b")
})

# ---------------------------------------------------------------------------- #
