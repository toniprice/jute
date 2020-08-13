library(jute)
context("notin")

# ---------------------------------------------------------------------------- #
test_that("Infix operator %notin% works for lhs smaller than rhs", {
  x <- c(1, 3)
  y <- c(2, 3, 4)
  expect_equal(x %notin% y, c(TRUE, FALSE))
})

# ---------------------------------------------------------------------------- #
test_that("Infix operator %notin% works for rhs smaller than lhs", {
  x <- c(2, 3, 4)
  y <- c(1, 3)
  expect_equal(x %notin% y, c(TRUE, FALSE, TRUE))
})

# ---------------------------------------------------------------------------- #
