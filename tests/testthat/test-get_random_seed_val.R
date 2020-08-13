library(jute)
context("get_random_seed_val")

# ---------------------------------------------------------------------------- #
test_that("get_random_seed_val runs without any issue", {
  expect_silent(get_random_seed_val())
})

# ---------------------------------------------------------------------------- #
