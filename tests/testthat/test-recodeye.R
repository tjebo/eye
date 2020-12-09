context("reveal")
library(eye)
library(eyedata)
library(testthat)

x_reg <- c("r", "re", "od", "right", "l", "le", "os", "left")
x_numna <- c(1:2, ".", NA, "", "    ")
x_weird <- c("alright", "righton", "lefty","leftover")
x_chrnum <- c(1:2, "r", "l")

test_that("no error",{
  expect_error(recodeye(x_reg), regexp = NA)
  expect_error(recodeye(1:2), regexp = NA)
  expect_error(recodeye(x_numna), regexp = NA)
  expect_error(recodeye(x_weird,eyecodes = list(c("alright","righton"), c("lefty","leftover"))), regexp = NA)

  })

test_that("warning/message",{
  expect_warning(recodeye(1:4), "NOT RECODED")
  expect_message(recodeye(c("r", 1:2)), "Introduced NA for unclear")
  expect_message(recodeye(x_weird), "Introduced NA for unclear")
  expect_message(recodeye(x_chrnum), "Introduced NA for unclear")
  expect_warning(recodeye(x_weird, dropunknown = FALSE), "NOT RECODED")
  expect_warning(recodeye(x_chrnum, dropunknown = FALSE), "NOT RECODED")
})

test_that("result", {
  expect_equal(sum(is.na(recodeye(1:2))), 0)
  expect_equal(sum(is.na(recodeye(0:1))), 0)
  expect_equal(sum(is.na(recodeye(x_numna))), 4)
})







