context("test eyes")
library(eye)
library(testthat)

source("./tests/testthat/eyes_testdata.R")

test_that("messages",{
  expect_message(eyes(foo1), regexp = "Not all eyes are identified \\(contains NA\\)")
  expect_message(eyes(foo6), "Eye coding somewhat messy")
  expect_message(eyes(foo8), "Eye coding somewhat messy")
  expect_message(eyes(foo10), "Eyes coded 0:1")
  expect_message(eyes(foo11), regexp = "Eyes coded 1:2")
  expect_message(eyes(foo17), regexp = "No eye column found")
  })

test_that("No warning",{
  expect_warning(eyes(foo2), regexp = NA)
  expect_warning(eyes(foo4), regexp = NA)
  expect_warning(eyes(foo5), regexp = NA)
  expect_warning(eyes(foo6), regexp = NA)
  expect_warning(eyes(foo8), regexp = NA)
  expect_warning(eyes(foo10), regexp = NA)
  expect_warning(eyes(foo11), regexp = NA)
})

test_that("Warnings", {
  expect_warning(eyes(foo13), "Eye coding ambiguous")
  expect_warning(eyes(foo14), "Eye coding ambiguous ")
  expect_warning(eyes(foo16), "Eye coding ambiguous ")
}
)

test_that("errors",{
  expect_error(eyes(foo0), "Eye coding ambiguous - guessing failed! Please clean data")
  expect_error(eyes(foo3), "Eye coding ambiguous - guessing failed! Please clean data")
  expect_error(eyes(foo7), "Could not identify patient or eye column")
  expect_error(eyes(foo9), "Could not identify patient or eye column")
  expect_error(eyes(foo12), "Eye coding ambiguous - guessing failed! Please clean data")
  expect_error(eyes(foo15), "Eye coding ambiguous - guessing failed! Please clean data")
  expect_error(eyes(foo18), "Patient column missing. Fix with \"id\" argument")
  expect_error(eyes(foo19), "Eye coding ambiguous - guessing failed! Please clean data")
  expect_error(eyes(foo20), "Eye coding ambiguous - guessing failed! Please clean data")
  expect_error(eyes(foo21), "Eye coding ambiguous - guessing failed! Please clean data")
})

sum_eyes <- function(x) {
  expect_equal(unname(eyes(x)["right"] + eyes(x)["left"]), unname(eyes(x)["eyes"]))
}

test_that("sum right and left", {
  sum_eyes(foo1)
  sum_eyes(foo2)
  sum_eyes(foo4)
  sum_eyes(foo5)
  sum_eyes(foo6)
  sum_eyes(foo6)
  sum_eyes(foo10)
  sum_eyes(foo11)
  sum_eyes(foo13)
  sum_eyes(foo14)
  sum_eyes(foo16)
})
