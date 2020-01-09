library(jute)
context("RNG utils")

# ============================================================================ #
# Tests
# ============================================================================ #

# ---------------------------------------------------------------------------- #
test_that("get_random_seed_val runs without any issue", {
  expect_silent(get_random_seed_val())
})

# ---------------------------------------------------------------------------- #
test_that("set_init_seed fails for non-integer seed value", {
  msg <- paste("'5aab' \\(note that this must be interpretable as an integer",
               "to set the seed)")
  expect_error(set_init_seed("5aab"), regexp = msg)
})

# ---------------------------------------------------------------------------- #
test_that("set_init_seed fails for vector seed value", {
  msg <- paste("Argument 'seed_val' should be an integer, the keyword",
               "'random', NULL or NA")
  expect_error(set_init_seed(c(1, 2)), regexp = msg)
})

# ---------------------------------------------------------------------------- #
test_that("set_init_seed accepts specific seed value without any issue", {
  expect_silent(set_init_seed(1))
})

# ---------------------------------------------------------------------------- #
test_that("set_init_seed accepts 'random' seed value without any issue", {
  expect_silent(set_init_seed("random"))
})

# ---------------------------------------------------------------------------- #
test_that("set_init_seed accepts 'NULL' seed value without any issue", {
  expect_silent(set_init_seed(NULL))
})

# ---------------------------------------------------------------------------- #
test_that("set_init_seed accepts 'NA' seed value without any issue", {
  expect_silent(set_init_seed(NA))
})

# ---------------------------------------------------------------------------- #
