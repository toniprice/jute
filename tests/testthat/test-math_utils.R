library(jute)
context("Math utils")

# ============================================================================ #
# Tests
# ============================================================================ #

# ---------------------------------------------------------------------------- #
test_that("is_int_val returns FALSE for non-integer value", {
  expect_false(is_int_val(5.2), "info: 5.2")
})

# ---------------------------------------------------------------------------- #
test_that("is_int_val returns TRUE for integer value", {
  expect_true(is_int_val(5),   "info: 5")
  expect_true(is_int_val(5.0), "info: 5.0")
})

# ---------------------------------------------------------------------------- #
test_that("is_sym_pos_def works as expected for sym pos def matrix", {

  spd_mtx <- matrix(c(2.51, 2.01, 2.01, 1.74), 2, 2)
  info <- paste(c(spd_mtx), collapse = ", ")
  expect_true(is_sym_pos_def(spd_mtx), sprintf("info: c(%s)", info))

  spd_mtx <- matrix(c(6.62, 3.33, 1.94, 3.33, 3.52, 0.5, 1.94, 0.5, 1.95), 3, 3)
  info <- paste(c(spd_mtx), collapse = ", ")
  expect_true(is_sym_pos_def(spd_mtx), sprintf("info: c(%s)", info))

  spd_mtx <- matrix(c(6.62, -1.8, 1.94, -1.8, 3.52, 0.5, 1.94, 0.5, 1.95), 3, 3)
  info <- paste(c(spd_mtx), collapse = ", ")
  expect_true(is_sym_pos_def(spd_mtx), sprintf("info: c(%s)", info))
})

# ---------------------------------------------------------------------------- #
test_that("is_sym_pos_def works as expected for non sym pos def matrix", {

  non_spd_mtx <- matrix(c(6.62, 4.9, 1.94, 4.9, 3.52, 0.5, 1.94, 0.5, 1.95),
                        3, 3)
  expect_false(is_sym_pos_def(non_spd_mtx))
})

# ---------------------------------------------------------------------------- #
test_that("is_sym_pos_def fails for non-symmetric matrix", {
  non_sym <- matrix(c(6.62, 3.33, 1.94, -4.1, 3.52, 0.5, 1.94, 0.5, 1.95), 3, 3)
  msg <- "Matrix is not symmetric"
  expect_error(is_sym_pos_def(non_sym), regexp = msg)
})

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
