library(jute)
context("pl")

# ---------------------------------------------------------------------------- #
test_that("pl works for regular singular", {
  expect_equal(pl("cow", 1), "cow")
})

# ---------------------------------------------------------------------------- #
test_that("pl works for regular plural", {
  expect_equal(pl("dog", 2), "dogs")
})

# ---------------------------------------------------------------------------- #
test_that("pl works for zero occurrences", {
  expect_equal(pl("dog", 0), "dogs")
})

# ---------------------------------------------------------------------------- #
test_that("pl works for irregular plural", {
  expect_equal(pl("sheep", 2, irregular = "sheep"), "sheep")
})

# ---------------------------------------------------------------------------- #
