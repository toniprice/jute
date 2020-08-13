library(jute)
context("stringify_vec")

# ---------------------------------------------------------------------------- #
test_that("stringify_vec works for unnamed vector", {
  expect_equal(stringify_vec(c(1, 2, 3)), "1, 2, 3")
})

# ---------------------------------------------------------------------------- #
test_that("stringify_vec works for named vector", {
  expect_equal(stringify_vec(c(a = 1, b = 2, c = 3)), "a = 1, b = 2, c = 3")
})

# ---------------------------------------------------------------------------- #
test_that("stringify_vec works with fmt", {
  vec <- c(alpha = 1, bravo = 2, charlie = 3)
  ex  <- "alpha = 1.0, bravo = 2.0, charlie = 3.0"
  expect_equal(stringify_vec(vec, fmt = "%3.1f"), ex)
})

# ---------------------------------------------------------------------------- #
test_that("stringify_vec works with non-default item_sep", {
  vec <- c(alpha = "a", bravo = "b")
  ex  <- "alpha = a;bravo = b"
  expect_equal(stringify_vec(vec, item_sep = ";"), ex)
})

# ---------------------------------------------------------------------------- #
test_that("stringify_vec works with non-default name_sep", {
  vec <- c(alpha = "a", bravo = "b")
  ex  <- "alpha: a, bravo: b"
  expect_equal(stringify_vec(vec, name_sep = ": "), ex)
})

# ---------------------------------------------------------------------------- #
