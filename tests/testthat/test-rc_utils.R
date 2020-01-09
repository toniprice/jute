library(jute)
context("Ref Class utils")

# ============================================================================ #
# RC class definitions
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# person_factory
# ---------------------------------------------------------------------------- #
person_factory <- setRefClass(

  "person",

  fields = list(
    name       = "character",
    birth_year = "integer"
  ),

  methods = list(
    say_hello = function() {
      "hello"
    }
  )
)

# ============================================================================ #
# Tests
# ============================================================================ #

# ---------------------------------------------------------------------------- #
test_that("get_rc_fld_vals retrieves fields as they have been set", {

  person <- person_factory$new(name = "Hans Rosling", birth_year = 1948L)

  act <- get_rc_fld_vals(person)
  ex  <- list(name = "Hans Rosling", birth_year = 1948L)

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("get_rc_fld_vals retrieves fields, overriding a value", {

  person <- person_factory$new(name = "Hans Rosling", birth_year = 1948L)

  # set name to Robert Plant, also born 1948 :)
  act <- get_rc_fld_vals(person, list(name = "Robert Plant"))
  ex  <- list(name = "Robert Plant", birth_year = 1948L)

  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
