context("test eyes")
library(eye)
library(testthat)

set.seed(42)
patient <- id <- c(letters[sample(20)], letters[sample(20)])
foo9 <- data.frame(patient, eyes = c("od", "le", "re", "os"), eye = "d")
foo8 <- data.frame(patient, eyes = c("od", "le", "re", "os"))
foo7 <- data.frame(patient, eyes = c("od", "le"), patience = "c")
foo6 <- data.frame(patient, eyes = c("od", "le", "re", "le"))
foo5 <- data.frame(patient , eyes = c("od", "le"))
foo3 <- data.frame(id , eyes = c("e", "l"))
foo4 <- data.frame(id, eyes = c("od", "le"))
foo2 <- data.frame(id, eyes = c("r", "l"))
foo1 <- data.frame(id, eyes = c(NA, "l"))


test_that("Warnings and errors", {
  expect_warning(eyes(foo1), "There are observations where the eyes are not identified")
  expect_warning(eyes(foo6), "Found several ways to code for right or left eyes")
  expect_error(eyes(foo7), "Patient and/or eye column(s) are not uniquely identified", fixed = TRUE)
})

test_that("str_length of factor is length of level", {
  expect_equal(str_length(factor("a")), 1)
  expect_equal(str_length(factor("ab")), 2)
  expect_equal(str_length(factor("abc")), 3)
})

test_that("str_length of missing is missing", {
  expect_equal(str_length(NA), NA_integer_)
  expect_equal(str_length(c(NA, 1)), c(NA, 1))
  expect_equal(str_length("NA"), 2)
})
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
