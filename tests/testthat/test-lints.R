library(jute)

# ---------------------------------------------------------------------------- #

run_lints <- TRUE

if (run_lints) {
  # see https://masalmon.eu/2017/06/17/automatictools/
  if (requireNamespace("lintr", quietly = TRUE)) {
    context("lints")
    test_that("Package Style", {
      lintr::expect_lint_free(relative_path = FALSE)
    })
  }
}

# ---------------------------------------------------------------------------- #
