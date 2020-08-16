library(jute)
context("notin")

# ---------------------------------------------------------------------------- #
test_that("Infix operator %notin% works for lhs with fewer elements than rhs", {
  x <- c(1, 3)
  y <- c(2, 3, 4)
  expect_equal(x %notin% y, c(TRUE, FALSE))
})

# ---------------------------------------------------------------------------- #
test_that("Infix operator %notin% works for rhs with fewer elements than lhs", {
  x <- c(2, 3, 4)
  y <- c(1, 3)
  expect_equal(x %notin% y, c(TRUE, FALSE, TRUE))
})

# ---------------------------------------------------------------------------- #
test_that("Infix operator %notin% works for strings", {
  x <- c("fox", "wolf", "dog")
  y <- c("the", "quick", "brown", "fox", "is", "not", "a", "dog")
  expect_equal(x %notin% y, c(FALSE, TRUE, FALSE))
})

# ---------------------------------------------------------------------------- #
