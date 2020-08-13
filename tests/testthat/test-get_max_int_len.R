library(jute)
context("get_max_int_len")

# ---------------------------------------------------------------------------- #
test_that("get_max_int_len works as expected", {

  expect_equal(get_max_int_len(1.0),           1, info = "1.0")
  expect_equal(get_max_int_len(2),             1, info = "2")
  expect_equal(get_max_int_len(c(2, 25, 467)), 3, info = "c(2, 25, 467)")
  expect_equal(get_max_int_len(c(51089, 2.0)), 5, info = "c(51089, 2.0)")
})

# ---------------------------------------------------------------------------- #
