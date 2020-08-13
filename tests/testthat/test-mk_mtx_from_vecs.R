library(jute)
context("mk_mtx_from_vecs")

# ---------------------------------------------------------------------------- #
test_that("mk_mtx_from_vecs works for 1-col numeric matrix", {
  act <- mk_mtx_from_vecs(x = c(10, 14, 3.4, 2, 26.0))
  ex  <- matrix(c(10, 14, 3.4, 2, 26.0), ncol = 1, dimnames = list(c(), c("x")))

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("mk_mtx_from_vecs works for 2-col character matrix", {
  act <- mk_mtx_from_vecs(col_1 = c("a", "b", "c"),
                          col_2 = c("alpha", "bravo", "charlie"))
  ex  <- matrix(c("a", "b", "c", "alpha", "bravo", "charlie"), ncol = 2,
                dimnames = list(c(), c("col_1", "col_2")))

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("mk_mtx_from_vecs works for unnamed vectors", {
  act <- mk_mtx_from_vecs(seq(10, 30, 10), seq(70, 90, 10))
  ex  <- matrix(c(10, 20, 30, 70, 80, 90), ncol = 2)

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
