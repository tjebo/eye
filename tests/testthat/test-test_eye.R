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
foo31 <- data.frame(id , eyes = c("e", "f"))
foo32 <- data.frame(id , eyes = c("r", "f"))
foo4 <- data.frame(id, eyes = c("od", "le"))
foo2 <- data.frame(id, eyes = c("r", "l"))
foo1 <- data.frame(id, eyes = c(NA, "l"))
foo0 <- data.frame(id, eyes = c(1:4))
foo10 <- data.frame(id, eyes = c(1:0))
foo11 <- data.frame(id, eyes = c(1:2))
foo111 <- data.frame(id, eyes = c(1,3))
foo12 <- data.frame(id, eyes = c(1, "r"))
foo13 <- data.frame(id, eyes = c(1, 2,"r", "r"))
foo14 <- data.frame(id, eyes = c(1, 2,"r", "l"))
foo15 <- data.frame(id, eyes = c(0, 2,"r", "l"))
foo16 <- data.frame(id, eyes = c(0, 1,"l", "l"))
foo17 <- data.frame(id)
foo91 <- data.frame(eyes = c(0, 1,"l", "l"))

eyes(foo12)

test_that("messages",{
  expect_message(eyes(foo17, "No eye column found in data nor specified"))
  expect_message(eyes(foo10), "Eyes are numerically coded.")
  expect_message(eyes(foo11), "Eyes are numerically coded.")
  expect_message(eyes(foo1), "Not all eyes are identified \\(contains NA\\)")
  })

test_that("Warnings", {

  expect_warning(eyes(foo6), "Eye coding ambiguous - guessing! Recommend to clean data")
  expect_warning(eyes(foo8), "Eye coding ambiguous - guessing! Recommend to clean data")
  expect_warning(eyes(foo3), "Eye coding ambiguous - guessing! Recommend to clean data")
  expect_warning(eyes(foo32), "Eye coding ambiguous - guessing! Recommend to clean data")
  expect_warning(eyes(foo12), "Eye coding ambiguous - guessing! Recommend to clean data")
  expect_warning(eyes(foo13), "Eye coding ambiguous - guessing! Recommend to clean data")
  expect_warning(eyes(foo14), "Eye coding ambiguous - guessing! Recommend to clean data")
  expect_warning(eyes(foo16), "Eye coding ambiguous - guessing! Recommend to clean data")
}
)

test_that("errors",{
  expect_error(eyes(foo7), "Could not identify patient or eye column", fixed = TRUE)
  expect_error(eyes(foo9), "Could not identify patient or eye column", fixed = TRUE)
  expect_error(eyes(foo91), "Patient column missing. Fix with \"id\" argument", fixed = TRUE)
  expect_error(eyes(foo0), "Eye coding ambiguous - guessing failed! Please clean data.", fixed = TRUE)
  expect_error(eyes(foo31), "Eye coding ambiguous - guessing failed! Please clean data.")
  expect_error(eyes(foo15), "Eye coding ambiguous - guessing failed! Please clean data.")
  expect_error(eyes(foo111), "Eye coding ambiguous - guessing failed! Please clean data.")
})

test_that("str_length of factor is length of level", {
  expect_equal(str_length(factor("a")), 1)
  expect_equal(str_length(factor("ab")), 2)
  expect_equal(str_length(factor("abc")), 3)
})

