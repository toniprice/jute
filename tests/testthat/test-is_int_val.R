library(jute)
context("is_int_val")

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
