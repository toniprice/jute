library(jute)
context("pad_c")

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
