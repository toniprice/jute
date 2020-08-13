library(jute)
context("cat_json_str")

# ---------------------------------------------------------------------------- #
test_that("cat_json_str works as expected", {

  txt <- paste("{\n    \"sig_lev\": [0.05],\n    \"sidedness\":",
               "[\"two\"],\n    \"n_sims\": [1000]\n  }")

  ex <- "{\"sig_lev\":[0.05],\"sidedness\":[\"two\"],\"n_sims\":[1000]}"

  expect_equal(cat_json_str(txt), ex)
})

# ---------------------------------------------------------------------------- #
