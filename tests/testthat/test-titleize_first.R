library(jute)
context("titleize_first")

# ---------------------------------------------------------------------------- #
test_that("titleize_first works for all lower case", {
  txt <- "a strange day indeed."
  expect_equal(titleize_first(txt), "A strange day indeed.")
})

# ---------------------------------------------------------------------------- #
test_that("titleize_first works for all title case", {
  txt <- "A strange day indeed."
  expect_equal(titleize_first(txt), "A strange day indeed.")
})

# ---------------------------------------------------------------------------- #
test_that("titleize_first works for all upper case", {
  txt <- "A STRANGE DAY INDEED."
  expect_equal(titleize_first(txt), "A STRANGE DAY INDEED.")
})

# ---------------------------------------------------------------------------- #
test_that("titleize_first works for one word", {
  txt <- "orange"
  expect_equal(titleize_first(txt), "Orange")
})

# ---------------------------------------------------------------------------- #
