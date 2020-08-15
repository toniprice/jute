library(jute)
context("logical_or")

# ---------------------------------------------------------------------------- #
test_that("Infix operator %||% works for NULL left operand", {
  expect_equal(NULL %||% 2, 2)
})

# ---------------------------------------------------------------------------- #
test_that("Infix operator %||% works for NULL right operand", {
  expect_equal(0 %||% NULL, 0)
})

# ---------------------------------------------------------------------------- #
test_that("Infix operator %||% works for NULL left and right operand", {
  expect_equal(NULL %||% NULL, NULL)
})

# ---------------------------------------------------------------------------- #
