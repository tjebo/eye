context("test clean_va and set_eye_strings")
library(eye)
library(testthat)

x_unclean <- c("pl", "lp", "HanD motion", "hand Movements", "perception of light",
               "count Fingers", "Cf", "NA", "nonsense", ".")


test_that("clean_va",{
  expect_equal(sum(is.na(clean_va(x_unclean))), 3)
  expect_in(clean_va(x_unclean)[!is.na(clean_va(x_unclean))], c("nlp", "lp",  "hm",  "cf"))
})

## set custom recognised value
set_eye_strings(hm = c(eye_codes$hm, "nonsense"))

test_that("clean_va",{
  expect_equal(sum(is.na(clean_va(x_unclean))), 2)
})

## revert back to normal
set_eye_strings()

test_that("clean_va",{
  expect_equal(sum(is.na(clean_va(x_unclean))), 3)
})
