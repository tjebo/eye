context("test va")
library(eye)
library(testthat)
## check how it works with NA !!!
logmar <- va_chart$logMAR
snellen_ft <- va_chart$snellen_ft
snellen_m <- va_chart$snellen_m
snellen_dec <- va_chart$snellen_dec
etdrs <- va_chart$ETDRS
etdrs_unplaus <- seq(-1,30,1)
logmar_unplaus <- seq(-4.4,2.0, 0.1)
snellen_unplaus <- letters[1:20]
quali <- va_chart$quali
quali_logmar <- c(logmar, quali)

test_that("error", {
  expect_error(va(logmar, from = "try"), "should be only one")
  expect_error(va(logmar, from = c("logmar, etdrs")), "should be only one")
  expect_error(va(logmar, to = c("logmar, etdrs")), "should be only one")
  expect_error(va(logmar, to = "logmar"), "VA already")
  expect_error(va(logmar), "VA already")
  expect_error(va(logmar, from = "ETDRS"), "class not plausible")
  expect_error(va(etdrs_unplaus), "Failed to autodetect")
  expect_error(va(logmar_unplaus), "Failed to autodetect")
  expect_error(va(snellen_unplaus), "Failed to autodetect")
})

test_that("no error", {
  expect_error(va(logmar, to = "ETDRS"), regexp = NA)
  expect_error(va(etdrs), regexp = NA)
  expect_error(va(snellen_dec), regexp = NA)
  expect_error(va(snellen_m), regexp = NA)
  expect_error(va(snellen_ft), regexp = NA)
})

test_that("message", {
  expect_message(va(etdrs), regexp = "Guessing ETDRS")
  expect_message(va(snellen_dec), "Guessing snellen")
})

test_that("warning", {
  expect_warning(eye:::which_va(etdrs_unplaus), "Guess ETDRS")
  expect_warning(eye:::which_va(logmar_unplaus), "Guess logMAR")
  expect_warning(eye:::which_va(snellen_unplaus), "Guess snellen")
  expect_warning(va(quali_logmar, to = "ETDRS"), "Removed characters")
})

test_that("return", {
  expect_identical(eye:::which_va(etdrs), "etdrs")
  expect_identical(eye:::which_va(snellen_dec), "snellen")
  expect_identical(eye:::which_va(snellen_m), "snellen")
  expect_identical(eye:::which_va(snellen_ft), "snellen")
  expect_identical(eye:::which_va(logmar), "logmar")
  expect_identical(eye:::which_va(quali), "quali")
  expect_identical(eye:::which_va(etdrs_unplaus), "failed")
  expect_identical(eye:::which_va(logmar_unplaus), "failed")
  expect_identical(eye:::which_va(snellen_unplaus), "failed")
})

