library(jute)
context("Lookup utils")

# ============================================================================ #
# Data
# ============================================================================ #

testlook <- mk_lookup_utils(
  list(
    numbers = c(
      french  = "un",
      spanish = "dos",
      italian = "tre"
    ),

    creatures = c(
      llama = "andes"
    ),

    epochs = c(
      kreide    = "Cretaceous",
      discovery = "Age of Discovery"
    )
  )
)

# ============================================================================ #
# Tests
# ============================================================================ #

# ---------------------------------------------------------------------------- #
test_that("get_all returns a list", {
  expect_is(testlook$get_all(), "list")
})

# ---------------------------------------------------------------------------- #
test_that("get_all return list contains character vectors", {
  all_lookups <- testlook$get_all()
  lapply(seq_along(all_lookups), function(i) {
    lk   <- all_lookups[[i]]
    info <- sprintf("info: lookup = %s", names(all_lookups)[i])
    expect_is(lk, "character", info)
  })
})

# ---------------------------------------------------------------------------- #
test_that("get_all return list of character vectors are all named", {
  all_lookups <- testlook$get_all()
  lapply(seq_along(all_lookups), function(i) {
    lk   <- all_lookups[[i]]
    info <- sprintf("info: lookup = %s", names(all_lookups)[i])
    expect_false(is.null(names(lk)), info = info)
  })
})

# ---------------------------------------------------------------------------- #
test_that("get fails for missing type", {
  msg <- "Argument 'type' cannot be missing"
  expect_error(testlook$get(), regex = msg)
})

# ---------------------------------------------------------------------------- #
test_that("get fails for nonexistent type", {
  msg <- "Unknown lookup type 'nonexistent'"
  expect_error(testlook$get("nonexistent"), regex = msg)
})

# ---------------------------------------------------------------------------- #
test_that("get succeeds for valid type", {
  expect_equal(testlook$get("epochs"),
               c(kreide = "Cretaceous", discovery = "Age of Discovery"))
})

# ---------------------------------------------------------------------------- #
test_that("descrip works as expected with type supplied", {
  act <- testlook$descrip("italian", "numbers")
  ex  <- "tre"
  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("descrip works as expected with lookup supplied", {
  act <- testlook$descrip("french", lookup = testlook$get("numbers"))
  ex  <- "un"
  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("descrip fails for missing key", {
  msg <- "Argument 'key' cannot be missing"
  expect_error(testlook$descrip(lookup = testlook$get("numbers")), regex = msg)
})

# ---------------------------------------------------------------------------- #
test_that("descrip fails for invalid key", {
  msg <- "Key 'danish' not found in lookup"
  expect_error(testlook$descrip("danish", "numbers"), regex = msg)
})

# ---------------------------------------------------------------------------- #
test_that("descrip fails if both type and lookup are NULL", {
  msg <- "Arguments 'type' and 'lookup' cannot both be NULL"
  expect_error(testlook$descrip("spanish"), regex = msg)
})

# ---------------------------------------------------------------------------- #
test_that("descrip retains name if requested", {
  act <- testlook$descrip("llama", "creatures", strip_name = FALSE)
  ex  <- c("llama" = "andes")
  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("descrip titleizes first letter if requested", {
  act <- testlook$descri("spanish", "numbers", tf = TRUE)
  ex  <- "Dos"
  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("descrip leaves first letter as-is if requested", {
  act <- testlook$descri("spanish", "numbers", tf = FALSE)
  ex  <- "dos"
  expect_equal(act, ex)
})

# ---------------------------------------------------------------------------- #
test_that("get_types retrieves all type names", {
  expect_equal(testlook$get_types(), c("numbers", "creatures", "epochs"))
})

# ---------------------------------------------------------------------------- #
test_that("get_choices retrieves all choice names", {
  expect_equal(testlook$get_choices("epochs"), c("kreide", "discovery"))
})

# ---------------------------------------------------------------------------- #
test_that("make works as expected", {
  f <- testlook$make("creatures")
  expect_equal(f("llama"), "andes")
})

# ---------------------------------------------------------------------------- #
test_that("make_all works as expected", {
  f1 <- testlook$make("numbers")
  f2 <- testlook$make("creatures")
  f3 <- testlook$make("epochs")

  ex <- list(lookup_numbers = f1, lookup_creatures = f2, lookup_epochs = f3)

  expect_equal(testlook$make_all(), ex)
})

# ---------------------------------------------------------------------------- #
test_that("add_to_env works for all lookup functions", {
  f1     <- testlook$make("numbers")
  f2     <- testlook$make("creatures")
  f3     <- testlook$make("epochs")

  ex <- list(lookup_numbers = f1, lookup_creatures = f2, lookup_epochs = f3)

  an_env <- new.env(parent = emptyenv())
  testlook$add_to_env(an_env)

  info <- "info: lookup_numbers exists"
  expect_true(exists("lookup_numbers", an_env), info)

  info <- "info: lookup_creatures exists"
  expect_true(exists("lookup_creatures", an_env), info)

  info <- "info: lookup_epochs exists"
  expect_true(exists("lookup_epochs", an_env), info)
})

# ---------------------------------------------------------------------------- #
test_that("add_to_env works for a subset of lookup functions", {
  f1     <- testlook$make("numbers")
  f3     <- testlook$make("epochs")

  lst    <- list(lookup_numbers = f1, lookup_epochs = f3)

  an_env <- new.env(parent = emptyenv())
  testlook$add_to_env(an_env, lst)

  expect_true(exists("lookup_numbers", an_env), "info: lookup_numbers exists")
  expect_true(exists("lookup_epochs",  an_env), "info: lookup_epochs exists")

  info <- "info: lookup_creatures does not exist"
  expect_false(exists("lookup_creatures", an_env), info)
})

# ---------------------------------------------------------------------------- #
