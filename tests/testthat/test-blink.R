context("global tests")
library(eye)
library(testthat)
#abraham <- read.csv("./data-raw/share_vision_df.csv")
data("amd")

test_that("no error", {
  #expect_error(myop(abraham), regexp = NA)
  expect_error(myop(amd), regexp = NA)
  expect_error(blink(amd), regexp = NA)
  #expect_error(blink(abraham), regexp = NA)
})

