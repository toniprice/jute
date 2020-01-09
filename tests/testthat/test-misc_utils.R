library(jute)
context("Misc utils")

# ============================================================================ #
# Tests
# ============================================================================ #

# ---------------------------------------------------------------------------- #
test_that("Infix operator %||% works for NULL left operand", {
  expect_equal(NULL %||% 2, 2)
})

# ---------------------------------------------------------------------------- #
test_that("Infix operator %||% works for NULL right operand", {
  expect_equal(0 %||% NULL, 0)
})

# ---------------------------------------------------------------------------- #
test_that("Infix operator %||% works for NULL left and right operand", {
  expect_equal(NULL %||% NULL, NULL)
})

# ---------------------------------------------------------------------------- #
test_that("Infix operator %notin% works for lhs smaller than rhs", {
  x <- c(1, 3)
  y <- c(2, 3, 4)
  expect_equal(x %notin% y, c(TRUE, FALSE))
})

# ---------------------------------------------------------------------------- #
test_that("Infix operator %notin% works for rhs smaller than lhs", {
  x <- c(2, 3, 4)
  y <- c(1, 3)
  expect_equal(x %notin% y, c(TRUE, FALSE, TRUE))
})

# ---------------------------------------------------------------------------- #
test_that("match_col_class matches character: stringsAsFactors = FALSE", {

  df <- data.frame(id = c("a", "b"), size = c(1.2, 3.5), num = c(1, 2),
                   numl = c(10L, 20L), stringsAsFactors = FALSE)

  expect_equal(match_col_class(df, "character"), c(id = 1))
})

# ---------------------------------------------------------------------------- #
test_that("match_col_class matches character: stringsAsFactors = TRUE", {

  df <- data.frame(id = c("a", "b"), size = c(1.2, 3.5), num = c(1, 2),
                   numl = c(10L, 20L), stringsAsFactors = TRUE)

  expect_equivalent(match_col_class(df, "character"), integer(0))
})

# ---------------------------------------------------------------------------- #
test_that("match_col_class matches factor", {

  df <- data.frame(id = c("a", "b"), size = c(1.2, 3.5), num = c(1, 2),
                   numl = c(10L, 20L), stringsAsFactors = TRUE)

  expect_equivalent(match_col_class(df, "factor"), c(id = 1))
})

# ---------------------------------------------------------------------------- #
test_that("match_col_class matches numeric", {

  df <- data.frame(id = c("a", "b"), size = c(1.2, 3.5), num = c(1, 2),
                   numl = c(10L, 20L))

  expect_equivalent(match_col_class(df, "numeric"), c(size = 2, num = 3))
})

# ---------------------------------------------------------------------------- #
test_that("match_col_class matches integer", {

  df <- data.frame(id = c("a", "b"), size = c(1.2, 3.5), num = c(1, 2),
                   numl = c(10L, 20L))

  expect_equivalent(match_col_class(df, "integer"), c(numl = 4))
})

# ---------------------------------------------------------------------------- #
test_that("mk_mtx works for 1-col numeric matrix", {
  act <- mk_mtx(x = c(10, 14, 3.4, 2, 26.0))
  ex  <- matrix(c(10, 14, 3.4, 2, 26.0), ncol = 1, dimnames = list(c(), c("x")))

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("mk_mtx works for 2-col character matrix", {
  act <- mk_mtx(col_1 = c("a", "b", "c"),
                col_2 = c("alpha", "bravo", "charlie"))
  ex  <- matrix(c("a", "b", "c", "alpha", "bravo", "charlie"), ncol = 2,
                dimnames = list(c(), c("col_1", "col_2")))

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("mk_mtx works for unnamed vectors", {
  act <- mk_mtx(seq(10, 30, 10), seq(70, 90, 10))
  ex  <- matrix(c(10, 20, 30, 70, 80, 90), ncol = 2)

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("simplify_lst_lst fails for unexpected list element", {

  a_1 <- "pineapple"
  a_2 <- matrix(NA, nrow = 2, ncol = 3)

  lst <- list(a_1, a_2)

  msg <- paste("Argument 'lst' should be a list of lists but found classes:",
               "character, matrix")
  expect_error(simplify_lst_lst(lst), regex = msg)
})

# ---------------------------------------------------------------------------- #
test_that("simplify_lst_lst works for 2 lists (both named)", {

  a_1 <- "pineapple"
  a_2 <- list(uno = 1, due = 2, tre = 3)
  lst <- list(list(z_1 = a_1), list(z_2 = a_2))

  ex  <- list(z_1 = "pineapple",
              z_2 = list(uno = 1, due = 2, tre = 3))

  expect_equal(simplify_lst_lst(lst), ex)
})

# ---------------------------------------------------------------------------- #
test_that("simplify_lst_lst works for 3 lists (none named, default prefix)", {

  a_1 <- "pineapple"
  a_2 <- list(uno = 1, due = 2, tre = 3)
  a_3 <- c(1, 4, 9, 16, 25)
  lst <- list(list(a_1), list(a_2), list(a_3))

  ex  <- list(name_1 = "pineapple",
              name_2 = list(uno = 1, due = 2, tre = 3),
              name_3 = c(1, 4, 9, 16, 25))

  expect_equal(simplify_lst_lst(lst), ex)
})

# ---------------------------------------------------------------------------- #
test_that("simplify_lst_lst works for 3 lists (only 2nd named, prefix given)", {

  a_1 <- "pineapple"
  a_2 <- list(uno = 1, due = 2, tre = 3)
  a_3 <- c(1, 4, 9, 16, 25)
  lst <- list(list(a_1), list(z = a_2), list(a_3))

  ex  <- list(y_1 = "pineapple",
              z   = list(uno = 1, due = 2, tre = 3),
              y_2 = c(1, 4, 9, 16, 25))

  expect_equal(simplify_lst_lst(lst, unnamed_prefix = "y"), ex)
})

# ---------------------------------------------------------------------------- #
