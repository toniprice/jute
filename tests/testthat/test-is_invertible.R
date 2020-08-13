library(jute)
context("is_invertible")

# ---------------------------------------------------------------------------- #
test_that("is_invertible works as expected for invertible matrix", {
  x <- matrix(c(1, 2, 2, 1), 2, 2)
  expect_true(is_invertible(x))
})

# ---------------------------------------------------------------------------- #
test_that("is_invertible fails for singular matrix", {
  x <- matrix(c(1, 1, 1, 1), 2, 2)
  expect_false(is_invertible(x))
})

# ---------------------------------------------------------------------------- #
