library(jute)
context("logical_to_str")

# ---------------------------------------------------------------------------- #
test_that("logical_to_str works for TRUE", {
  expect_equal(logical_to_str(TRUE), "Yes")
})

# ---------------------------------------------------------------------------- #
test_that("logical_to_str works for FALSE", {
  expect_equal(logical_to_str(FALSE), "No")
})

# ---------------------------------------------------------------------------- #
test_that("logical_to_str works for title case output", {
  expect_equal(logical_to_str(TRUE, "title"), "Yes")
})

# ---------------------------------------------------------------------------- #
test_that("logical_to_str works for lower case output", {
  expect_equal(logical_to_str(TRUE, "lower"), "yes")
})

# ---------------------------------------------------------------------------- #
test_that("logical_to_str works for upper case output", {
  expect_equal(logical_to_str(TRUE, "upper"), "YES")
})

# ---------------------------------------------------------------------------- #
