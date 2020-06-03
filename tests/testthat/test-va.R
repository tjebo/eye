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
  expect_error(va(logmar, to = c("logmar, etdrs")), "\"to\": Pick one of")
  expect_error(va(snellen_unplaus), "Failed to detect")
})

test_that("no error / no warning", {
  expect_error(va(mixed_VA), regexp = NA)
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
  expect_warning(va(etdrs_logmar), "Wavering between logmar and etdrs")
  expect_warning(va(etdrs_logmar_na), "Wavering between logmar and etdrs")
  expect_warning(va(snellen_logmar), "Wavering between logmar and snellen")
  expect_warning(va(snellen_logmar_na), "Wavering between logmar and snellen")
  expect_warning(va(etdrs_logmar, from = "etdrs"), regexp = NA)
  expect_warning(va(etdrs_logmar, from = "ETDRS"), regexp = NA)
  expect_warning(va(snellen_logmar, from = "Snellen"), regexp = NA)
  expect_warning(va(snellen_logmar, from = "snellen"), regexp = NA)
})

test_that("message", {
  expect_message(va(mixed_VA), "Mixed object")
  expect_message(va(etdrs), regexp = "From etdrs")
  expect_message(va(etdrs, from = "etdrs"), regexp = NA)
  expect_message(va(snellen_dec), "From snellen")
  expect_message(va(quali_snellen_ft, from = "etdrs"), "From snellen")
  expect_message(va(quali_snellen_ft, from = "snellen"),regexp = NA)
  expect_message(va(va_vec3, to = "etdrs"), "VA already")
  expect_message(va(va_vec2, to = "logmar"), "VA already")
  expect_message(va(logmar, to = "logmar"), "VA already")
  expect_message(va(logmar, from = "ETDRS"), "VA already")
  expect_message(va(logmar_unplaus), "VA already")
  expect_message(va(logmar), "From logmar")
})

test_that("warning", {
  expect_warning(va(logmar, from = c("logmar, etdrs")), "Ignoring \"from\": logmar, etdrs implausible")
  expect_warning(va(logmar, from = "try"), "Ignoring \"from\": try implausible")
  expect_warning(va(logmar, from = "ETDRS"), "Ignoring \"from\": ETDRS implausible")
  expect_warning(va(quali_snellen_ft, from = "ETDRS"),"Ignoring \"from\": ETDRS implausible")
  expect_warning(va(va_vec2, to = "snellen", from = "etdrs"), "Ignoring \"from\": overriden by VA class")
  expect_warning(va(amd$VA_ETDRS_Letters, from = 'etdrs'), "Implausible values")
  expect_warning(va(etdrs_unplaus), "Implausible values")
  expect_warning(va(logmar_unplaus), "Implausible values")
  expect_warning(va(etdrs_logmar, from = "snellen"), "Ignoring \"from\"")
  expect_warning(va(logmar, to = "snellen", snellen_type = "random"), "Ignoring snellen_type")

})

test_that("return", {
  expect_identical(eye:::which_va(etdrs), "etdrs")
  expect_identical(eye:::which_va(snellen_dec), "snellen")
  expect_identical(eye:::which_va(snellen_m), "snellen")
  expect_identical(eye:::which_va(snellen_ft), "snellen")
  expect_identical(eye:::which_va(logmar), "logmar")
  expect_identical(eye:::which_va(quali), "quali")
  expect_identical(eye:::which_va(etdrs_unplaus), c("etdrs", "implaus"))
  expect_identical(eye:::which_va(logmar_unplaus), c("logmar", "implaus"))
  expect_identical(eye:::which_va(snellen_unplaus), "failed")
  expect_identical(eye:::which_va(quali_logmar), "logmar")
  expect_identical(eye:::which_va(quali_snellen_ft), "snellen")
  expect_identical(eye:::which_va(quali_snellen_m), "snellen")
  expect_identical(eye:::which_va(etdrs_logmar), c("logmar","etdrs"))
  expect_identical(eye:::which_va(snellen_logmar), c("logmar","snellen"))
  expect_true(inherits(va(va_vec2, to = "etdrs"), "etdrs"))
  expect_true(inherits(va(va_vec), "logmar"))
  expect_length(va(va_vec), length(va_vec))
  expect_length(va(va_vec1), length(va_vec1))
  expect_length(va(va_vec2), length(va_vec2))
  expect_length(va(va_vec3), length(va_vec3))
  expect_length(va(va_vec4), length(va_vec4))
  expect_length(va(va_vec5), length(va_vec5))
  expect_true(all(grepl("20/", va(logmar, to = "snellen"))))
  expect_true(all(grepl("20/", va(logmar, to = "snellen", snellen_type = "random"))))
  expect_true(all(grepl("6/", va(logmar, to = "snellen", snellen_type = "m"))))
  expect_true(all(va(logmar, to = "snellen", snellen_type = "dec") %in% va_chart$snellen_dec))
})


test_that("No error / no warning: va_dissect", {
  expect_warning(va_dissect(va_vec), regexp = NA)
  expect_warning(va_dissect(va_vec1), regexp = NA)
  expect_warning(va_dissect(va_vec2), regexp = NA)
  expect_warning(va_dissect(va_vec3), regexp = NA)
  expect_warning(va_dissect(va_vec4), regexp = NA)
  expect_warning(va_dissect(va_vec5), regexp = NA)
  expect_error(va_dissect(va_vec), regexp = NA)
  expect_error(va_dissect(va_vec1), regexp = NA)
  expect_error(va_dissect(va_vec2), regexp = NA)
  expect_error(va_dissect(va_vec3), regexp = NA)
  expect_error(va_dissect(va_vec4), regexp = NA)
  expect_error(va_dissect(va_vec5), regexp = NA)
  expect_warning(va_dissect(mixed_VA2), regexp = NA)
}
)

test_that("Warning: va_dissect", {
  expect_warning(va_dissect(mixed_VA), "Implausible")
  expect_warning(va_dissect(mixed_VA), "Snellen?")
  expect_warning(va_dissect(mixed_VA1), "Implausible")
}
)

