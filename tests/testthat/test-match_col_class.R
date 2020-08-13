library(jute)
context("match_col_class")

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
