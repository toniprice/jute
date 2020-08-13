library(jute)
context("simplify_lst_lst")

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
