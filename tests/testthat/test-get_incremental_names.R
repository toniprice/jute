library(jute)
context("get_incremental_names")

# ---------------------------------------------------------------------------- #
test_that("get_incremental_names works for default sep", {
  nms <- c("item_1", "item_2", "item_3")
  expect_equal(get_incremental_names(3, "item"), nms)
})

# ---------------------------------------------------------------------------- #
test_that("get_incremental_names works for blank sep", {
  expect_equal(get_incremental_names(2, "item", sep = ""), c("item1", "item2"))
})

# ---------------------------------------------------------------------------- #
test_that("get_incremental_names works for more than 9 items", {
  ex <- c("x_01", "x_02", "x_03", "x_04", "x_05", "x_06", "x_07", "x_08",
          "x_09", "x_10", "x_11")
  expect_equal(get_incremental_names(11, "x"), ex)
})

# ---------------------------------------------------------------------------- #
