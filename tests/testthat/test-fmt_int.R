library(jute)
context("fmt_int")

# ---------------------------------------------------------------------------- #
test_that("fmt_int formats single integers as expected", {
  expect_equal(fmt_int(5.0), "5", info = "info: 5.0")
  expect_equal(fmt_int(6),   "6", info = "info: 6")
})

# ---------------------------------------------------------------------------- #
test_that("fmt_int gives error for non-integer value", {
  msg <- "Expected integer value but received '4.3'"
  expect_error(fmt_int(4.3), regexp = msg)
})

# ---------------------------------------------------------------------------- #
test_that("fmt_int formats a vector of integers as expected", {
  expect_equal(fmt_int(c(1, 25, 467)), c("  1", " 25", "467"),
               info = "info: c(1, 25, 467)")
})

# ---------------------------------------------------------------------------- #
test_that("fmt_int gives error for non-integer value in vector", {
  msg <- "Expected integer value but received '4.3'"
  expect_error(fmt_int(c(1, 4.3, 5)), regexp = msg)
})

# ---------------------------------------------------------------------------- #
