context("test va")
library(eye)
library(testthat)
source("./tests/testthat/va_testdata.R")

# $va_vec (length 8)
# [1] "NLP" "LP"  "HM"  "CF"  "6/60""20/200" "6/9" "20/40"
# $va_vec1 (length 10)
# "NPL" "NLP" "LP"  "PL"  "HM"  "CF"  "6/60""20/200" "6/9""20/40"
# $va_vec2 (length 8)
# [1] 3.00 2.70 2.30 1.90 1.00 1.00 0.18 0.30
# $va_vec3 (length 8)
# [1]  0  0  0  2 35 35 75 70
# $va_vec4 (length 13)
# [1] NA  "CF""NA""HM""6/9"  "6/6"  "6/18" "6/12" "6/5"  "6/24" "6/36" "6/60" "3/60"
# $va_vec5 (length 5)
# [1] "20/200"  "20/200 + 3" "20/200+3""20/200-4""20/200 -4"

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
  expect_warning(va(logmar, to = "logmar"), regexp = NA)
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
  expect_message(va(va_vec2, to = "logmar"), "VA is already")
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
  expect_length(va(va_vec), length(va_vec))
  expect_length(va(va_vec1), length(va_vec1))
  expect_length(va(va_vec2), length(va_vec2))
  expect_length(va(va_vec3), length(va_vec3))
  expect_length(va(va_vec4), length(va_vec4))
  expect_length(va(va_vec5), length(va_vec5))
})

