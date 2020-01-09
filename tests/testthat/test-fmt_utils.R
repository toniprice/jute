library(jute)
context("Format utils")

# ============================================================================ #
# Tests
# ============================================================================ #

# ---------------------------------------------------------------------------- #
test_that("pad_c works for 1 vector", {

  vec1 <- c("Avocado:", "Blueberry:", "Coconut:", "Dill:")
  act  <- pad_c(vec1)

  ex <- c("Avocado:",
          "Blueberry:",
          "Coconut:",
          "Dill:")

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("pad_c works for 2 vectors", {

  vec1 <- c("Avocado:", "Blueberry:", "Coconut:", "Dill:")
  vec2 <- c(rep("fruit", 3), "herb")
  act  <- pad_c(vec1, vec2)

  ex <- c("Avocado:   fruit",
          "Blueberry: fruit",
          "Coconut:   fruit",
          "Dill:      herb")

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("pad_c works for 3 vectors", {

  vec1 <- c("Avocado:", "Blueberry:", "Coconut:", "Dill:")
  vec2 <- c(rep("fruit", 3), "herb")
  vec3 <- c("green", "purple", "white", "green")
  act  <- pad_c(vec1, vec2, vec3)

  ex <- c("Avocado:   fruit green",
          "Blueberry: fruit purple",
          "Coconut:   fruit white",
          "Dill:      herb  green")

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("pad_c works for 1 vector with non-default collapse", {

  vec1 <- c("Avocado:", "Blueberry:", "Coconut:", "Dill:")
  act  <- pad_c(vec1, collapse = "#")

  ex <- "Avocado:#Blueberry:#Coconut:#Dill:"

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("pad_c works for 2 vectors with non-default collapse", {

  vec1 <- c("Avocado:", "Blueberry:", "Coconut:", "Dill:")
  vec2 <- c(rep("fruit", 3), "herb")
  act  <- pad_c(vec1, vec2, collapse = "; ")

  ex <- "Avocado:   fruit; Blueberry: fruit; Coconut:   fruit; Dill:      herb"

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("pad_c works for 2 vectors with newline collapse", {

  vec1 <- c("Avocado:", "Blueberry:", "Coconut:", "Dill:")
  vec2 <- c(rep("fruit", 3), "herb")
  act  <- pad_c(vec1, vec2, collapse = "\n")

  ex <- "Avocado:   fruit\nBlueberry: fruit\nCoconut:   fruit\nDill:      herb"

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("pad_c works for 2 vectors with non-default sep", {

  vec1 <- c("Avocado:", "Blueberry:", "Coconut:", "Dill:")
  vec2 <- c(rep("fruit", 3), "herb")
  act  <- pad_c(vec1, vec2, sep = " #")

  ex <- c("Avocado:   #fruit",
          "Blueberry: #fruit",
          "Coconut:   #fruit",
          "Dill:      #herb")

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("pad_c works for 1 vector with padding applied to all vecs", {

  vec1 <- c("Avocado:", "Blueberry:", "Coconut:", "Dill:")
  act  <- pad_c(vec1, pad_last = TRUE)

  ex <- c("Avocado:  ",
          "Blueberry:",
          "Coconut:  ",
          "Dill:     ")

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("pad_c works for 1 vec with all vecs padded + non-default collapse", {

  vec1 <- c("Avocado:", "Blueberry:", "Coconut:", "Dill:")
  act  <- pad_c(vec1, pad_last = TRUE, collapse = "#")

  ex <- "Avocado:  #Blueberry:#Coconut:  #Dill:     "

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("pad_c works for 3 vectors with padding applied to all vecs", {

  vec1 <- c("Avocado:", "Blueberry:", "Coconut:", "Dill:")
  vec2 <- c(rep("fruit", 3), "herb")
  vec3 <- c("green", "purple", "white", "green")
  act  <- pad_c(vec1, vec2, vec3, pad_last = TRUE)

  ex <- c("Avocado:   fruit green ",
          "Blueberry: fruit purple",
          "Coconut:   fruit white ",
          "Dill:      herb  green ")

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("pad_c works for 3 vectors with newline collapse", {

  vec1 <- c("Avocado:", "Blueberry:", "Coconut:", "Dill:")
  vec2 <- c(rep("fruit", 3), "herb")
  vec3 <- c("green", "purple", "white", "green")
  act  <- pad_c(vec1, vec2, vec3, collapse = "\n")

  ex <- paste0("Avocado:   fruit green\n",
               "Blueberry: fruit purple\n",
               "Coconut:   fruit white\n",
               "Dill:      herb  green")

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("fmt_int formats single integers as expected", {
  expect_equal(fmt_int(5.0), "5", info = "info: 5.0")
  expect_equal(fmt_int(6),   "6", info = "info: 6")
})

# ---------------------------------------------------------------------------- #
test_that("fmt_int gives error for non-integer value", {
  msg <- "Expected integer value but received '4.3'"
  expect_error(fmt_int(4.3), regexp = msg)
})

# ---------------------------------------------------------------------------- #
test_that("fmt_int formats a vector of integers as expected", {
  expect_equal(fmt_int(c(1, 25, 467)), c("  1", " 25", "467"),
               info = "info: c(1, 25, 467)")
})

# ---------------------------------------------------------------------------- #
test_that("fmt_int gives error for non-integer value in vector", {
  msg <- "Expected integer value but received '4.3'"
  expect_error(fmt_int(c(1, 4.3, 5)), regexp = msg)
})

# ---------------------------------------------------------------------------- #
test_that("get_max_int_len works as expected", {

  expect_equal(get_max_int_len(1.0),           1, info = "1.0")
  expect_equal(get_max_int_len(2),             1, info = "2")
  expect_equal(get_max_int_len(c(2, 25, 467)), 3, info = "c(2, 25, 467)")
  expect_equal(get_max_int_len(c(51089, 2.0)), 5, info = "c(51089, 2.0)")
})

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
test_that("lgl_str works for TRUE", {
  expect_equal(lgl_str(TRUE), "Yes")
})

# ---------------------------------------------------------------------------- #
test_that("lgl_str works for FALSE", {
  expect_equal(lgl_str(FALSE), "No")
})

# ---------------------------------------------------------------------------- #
test_that("lgl_str works for title case output", {
  expect_equal(lgl_str(TRUE, "title"), "Yes")
})

# ---------------------------------------------------------------------------- #
test_that("lgl_str works for lower case output", {
  expect_equal(lgl_str(TRUE, "lower"), "yes")
})

# ---------------------------------------------------------------------------- #
test_that("lgl_str works for upper case output", {
  expect_equal(lgl_str(TRUE, "upper"), "YES")
})

# ---------------------------------------------------------------------------- #
