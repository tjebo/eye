context("test va")
library(eye)
library(testthat)
source("./tests/testthat/va_testdata.R")

test_that("error", {
  expect_error(va(logmar, from = "try"), "should be only one")
  expect_error(va(logmar, from = c("logmar, etdrs")), "should be only one")
  expect_error(va(logmar, to = c("logmar, etdrs")), "should be only one")
  expect_error(va(snellen_unplaus), "Failed to detect")
  expect_error(va(logmar, from = "ETDRS"), "class not plausible")
  expect_error(va(quali_snellen_ft,from = "ETDRS"), "class not plausible")
})

test_that("no error / warning", {
  expect_error(va(logmar, to = "ETDRS"), regexp = NA)
  expect_error(va(etdrs), regexp = NA)
  expect_error(va(snellen_dec), regexp = NA)
  expect_error(va(logmar, to = "logmar"), regexp = NA)
  expect_error(va(snellen_m), regexp = NA)
  expect_error(va(snellen_ft), regexp = NA)
  expect_warning(va(quali_logmar, to = "ETDRS"), regexp = NA)
  expect_warning(va(quali_snellen_ft, to = "ETDRS"), regexp = NA)
  expect_warning(va(va_vec), regexp = NA)
  expect_warning(va(va_vec2, to = "snellen"), regexp = NA)
  expect_warning(va(va_vec3), regexp = NA)

})

test_that("message", {
  expect_message(va(etdrs), regexp = "Guessing ETDRS")
  expect_message(va(snellen_dec), "Guessing snellen")
  expect_message(va(va_vec3, to = "etdrs"), "VA is already")
  expect_message(va(logmar, to = "logmar"), "VA was already")
})

test_that("warning", {
  expect_warning(eye:::which_va(etdrs_unplaus), "Guess ETDRS")
  expect_warning(eye:::which_va(logmar_unplaus), "Guess logMAR")
  expect_warning(eye:::which_va(snellen_unplaus), "Guess snellen")
  expect_warning(va(va_vec2, to = "snellen", from = "etdrs"), "from argument")
  expect_warning(va(amd$VA_ETDRS_Letters, from = 'etdrs'), "Values out of range")
  expect_warning(va(etdrs_unplaus), "Values out of range")
  expect_warning(va(logmar_unplaus), "Values out of range")
})

test_that("return", {
  expect_identical(eye:::which_va(etdrs), "etdrs")
  expect_identical(eye:::which_va(snellen_dec), "snellen")
  expect_identical(eye:::which_va(snellen_m), "snellen")
  expect_identical(eye:::which_va(snellen_ft), "snellen")
  expect_identical(eye:::which_va(logmar), "logmar")
  expect_identical(eye:::which_va(quali), "quali")
  expect_identical(eye:::which_va(etdrs_unplaus), "etdrs")
  expect_identical(eye:::which_va(logmar_unplaus), "logmar")
  expect_identical(eye:::which_va(snellen_unplaus), "failed")
  expect_identical(eye:::which_va(quali_logmar), "logmar")
  expect_identical(eye:::which_va(quali_snellen_ft), "snellen")
  expect_identical(eye:::which_va(quali_snellen_m), "snellen")
  expect_true(inherits(va(va_vec2, to = "etdrs"), "etdrs"))
  expect_true(inherits(va(va_vec), "logmar"))
})

