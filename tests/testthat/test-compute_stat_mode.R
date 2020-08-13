library(jute)
context("compute_stat_mode")

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode fails for non-atomic type", {
  msg <- paste("Supplied values need to be atomic of type:",
               "factor | character | integer | logical | numeric")
  x <- list(matrix(1:9, 3, 3), matrix(1:4, 2, 2))
  expect_error(compute_stat_mode(x), regexp = msg)
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for integer values", {
  expect_equal(compute_stat_mode(c(1, 1, 3, 5, 1, 3, 1, 2)), 1)
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for integer values with one duplicate", {
  expect_equal(compute_stat_mode(c(1, 3, 5, 3, 0)), 3)
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for integer values with no mode", {
  expect_equal(compute_stat_mode(c(1, 3, 5, 2)), NA)
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for integer values with 2 modes", {
  expect_equal(compute_stat_mode(c(63, 62, 66, 67, 63, 70, 67, 68, 61)),
               c(63, 67))
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for a sample from a continuous RV", {
  set.seed(10)
  expect_equal(compute_stat_mode(rnorm(100)), -0.1795327, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for logical values", {
  expect_equal(compute_stat_mode(c(TRUE, FALSE, FALSE, TRUE, FALSE)), FALSE)
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for logical values with no mode", {
  expect_equal(compute_stat_mode(c(FALSE, TRUE)), NA)
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for characters", {
  expect_equal(compute_stat_mode(c("a", "b", "c", "c")), "c")
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for characters with no mode", {
  expect_equal(compute_stat_mode(c("a", "b", "c", "d")), NA)
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for character strings", {
  expect_equal(compute_stat_mode(c("dog", "buffalo", "cat", "buffalo")),
               "buffalo")
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for character strings with no mode", {
  expect_equal(compute_stat_mode(c("dog", "buffalo", "cat")), NA)
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for factor values", {
  expect_equal(compute_stat_mode(as.factor(c("a", "b", "c", "c"))), "c")
})

# ---------------------------------------------------------------------------- #
test_that("compute_stat_mode works for factor values with no mode", {
  expect_equal(compute_stat_mode(as.factor(c("a", "b", "c", "d"))), NA)
})

# ---------------------------------------------------------------------------- #
