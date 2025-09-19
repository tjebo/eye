context("test va")
library(eye)
library(testthat)

source("tests/testthat/va_testobjects.R")

test_that("output", {
  expect_equal(va(logmar), logmar)
  expect_equal(va(etdrs), etdrs)
  expect_equal(va(snellendec), as.double(snellendec))
  expect_equal(va(snellenft), snellenft)
  expect_equal(va(snellenm), snellenm)
  expect_equal(va(va_vec4), c(NA, "cf", NA, "hm", "6/9", "6/6", "6/18", "6/12", "6/5", "6/24",
                              "6/36", "6/60", "3/60"))
  expect_equal(va(va_vec5), c("20/200", "20/200+3", "20/200+3", "20/200-4", "20/200-4"))
  expect_equal(va(va_vec6), c("6/9", "6/6", "6/5", "6/12", "6/7.5", "6/18", "lp", "6/4",
                              "6/24", "6/36", "hm", "6/60", NA, "cf", NA, "6/2", "nlp", NA,
                              NA, "6/3", NA, "6/7"))
  expect_equal(sum(is.na(va(va_vec7))), 1)
  expect_equal(sum(is.na(va(va_vec))), 2)
  expect_equal(sum(is.na(va(va_vec1))), 0)
  expect_true(all(is.numeric(va(va_vec2))))
  expect_equal(sum(is.na(va(etdrs_unplaus, to = "logmar"))), sum(etdrs_unplaus <0))
  expect_equal(sum(is.na(va(logmar_unplaus, to = "logmar"))), sum(logmar_unplaus< -0.3))
})

test_that("no error / no warning", {
  expect_error(va(logmar, to = "etdrs"), regexp = NA)
  expect_error(va(etdrs), regexp = NA)
  expect_error(va(snellendec), regexp = NA)
  expect_error(va(logmar, to = "logmar"), regexp = NA)
  expect_warning(va(logmar, to = "logmar"), regexp = NA)
  expect_error(va(snellenm), regexp = NA)
  expect_error(va(snellenft), regexp = NA)
  expect_warning(va(quali_logmar, to = "etdrs"), regexp = NA)
  expect_warning(va(quali_snellen_ft, to = "etdrs"), regexp = NA)
  expect_warning(va(va_vec), regexp = NA)
  expect_warning(va(va_vec2, to = "snellen"), regexp = NA)
  expect_warning(va(va_vec3), regexp = NA)

})

test_that("message", {
  expect_message(suppressWarnings(va(mixed_VA)), "2x NA introduced for: NA, eye")
  expect_message(suppressWarnings(va(mixed_VA1)), "1x NA introduced for: NA")
  expect_message(va(mixed_VA2), "1x NA introduced for: NA")
  expect_message(va(etdrs, to = "logmar"), regexp = "From etdrs")
  expect_message(va(snellendec, from = "snellendec", to = "logmar"), regexp = NA)
  expect_message(va(quali_snellen_ft, to = "logmar"), "From snellen")
  expect_message(va(logmar, to = "etdrs"), "From logmar")
  expect_message(va(etdrs_logmar, to = "logmar"), "From etdrs. Could be")
  expect_message(va(etdrs_logmar_na, to = "logmar"), "From etdrs. Could be")
  expect_message(va(snellen_logmar, to = "etdrs"), "From logmar")
  expect_message(va(snellen_logmar_na, to = "etdrs"), "From logmar")
  expect_message(va(snellen_unplaus), "20x NA introduced for")
  expect_message(va(etdrs_unplaus, to = "logmar"), "1x NA introduced for: -1")
  expect_message(va(logmar_unplaus, to = "logmar"), "From logmar")
  expect_message(va(logmar_unplaus, to = "logmar"), "41x NA introduced for")
})


test_that("return", {
  expect_true(inherits(va(va_vec2, to = "etdrs"), "etdrs"))
  expect_length(va(va_vec), length(va_vec))
  expect_length(va(va_vec1), length(va_vec1))
  expect_length(va(va_vec2), length(va_vec2))
  expect_length(va(va_vec3), length(va_vec3))
  expect_length(va(va_vec4), length(va_vec4))
  expect_length(va(va_vec5), length(va_vec5))
  expect_true(all(grepl("20/", va(logmar, to = "snellen"))))
  expect_true(suppressWarnings(all(grepl("20/", va(logmar, to = "snellen", type = "random")))))
  expect_true(all(grepl("6/", va(logmar, to = "snellen", type = "m"))))
  expect_true(all(va(logmar, to = "snellen", type = "dec") %in% va_chart$snellendec))
  expect_true(inherits(va(va_vec, to = "logmar"), "logmar"))
  expect_true(suppressWarnings(inherits(va(mixed_VA, to = "logmar"), "logmar")))
})


test_that("No error / no warning", {
  expect_warning(va(va_vec), regexp = NA)
  expect_warning(va(va_vec1), regexp = NA)
  expect_warning(va(va_vec2), regexp = NA)
  expect_warning(va(va_vec3), regexp = NA)
  expect_warning(va(va_vec4), regexp = NA)
  expect_warning(va(va_vec5), regexp = NA)
  expect_error(va(va_vec), regexp = NA)
  expect_error(va(va_vec1), regexp = NA)
  expect_error(va(va_vec4), regexp = NA)
  expect_error(va(va_vec2), regexp = NA)
  expect_error(va(va_vec3), regexp = NA)
  expect_error(va(va_vec5), regexp = NA)
  expect_warning(va(mixed_VA2), regexp = NA)
}
)

test_that("NA_va", {

  expect_equal(sum(is.na(va(va_vec))), 2)
  expect_equal(sum(is.na(va(va_vec4))), 2)
  expect_equal(sum(is.na(va(va_vec6))), 5)
  expect_equal(suppressWarnings(sum(is.na(va(va_vecNA)))), 2)
  expect_equal(sum(is.na(va(va_vec1))), 0)
  expect_equal(sum(is.na(va(va_vec2))), 0)
  expect_equal(sum(is.na(va(va_vec3))), 0)
  expect_equal(sum(is.na(va(va_vec5))), 0)
  expect_equal(sum(is.na(va(va_vec8, to = "etdrs"))), 26)
  expect_equal(sum(is.na(va(va_vec8, to = "snellen", type = "dec"))), 26)
  expect_equal(sum(is.na(va(c(25, 23, 0.4), to = "snellen"))), 2) #class quali
  expect_equal(suppressWarnings(sum(is.na(va(mixed_VA2)))), 2)
  expect_equal(suppressWarnings(sum(is.na(va(mixed_VA)))), 3)
  expect_equal(suppressWarnings(sum(is.na(va(mixed_VA1)))), 2)
  expect_equal(sum(is.na(va(xtry, to = "snellen", from = "logmar"))), 6)
  expect_equal(sum(is.na(va(xtry, to = "snellen", from = "etdrs"))), 6)
  expect_equal(sum(is.na(va(xtry, to = "snellen", from = "snellen"))), 6)
  expect_equal(sum(is.na(va(xtry, to = "snellen", from = "snellendec"))), 6)
  }
)

test_that("NA_convert",{
  expect_equal(sum(is.na(eye:::convertVA(va_vec2, to = "snellen", "ft"))),0) #class logmar
  expect_equal(sum(is.na(eye:::convertVA(va_vec2, to = "logmar", "ft"))),0) #class logmar
  expect_equal(sum(is.na(eye:::convertVA(va_vec3, to = "etdrs", "ft"))),0) #class etdrs
  expect_equal(sum(is.na(eye:::convertVA(va_vec3, to = "snellen", "ft"))),0) #class etdrs
  expect_equal(sum(is.na(eye:::convertVA(va_vec3, to = "logmar", "ft"))),0) #class etdrs
  expect_equal(sum(is.na(eye:::convertVA(va_vec7, smallstep = FALSE, noplus = FALSE, to = "snellen", "dec"))), 1) #class snellen
  expect_equal(sum(is.na(eye:::convertVA(va_vec7,  smallstep = FALSE,noplus = FALSE,to = "etdrs", "ft"))),1) #class snellen
  expect_equal(sum(is.na(eye:::convertVA(va_vec7, smallstep = FALSE, noplus = FALSE,to = "logmar", "ft"))),1) #class snellen
  expect_equal(sum(is.na(eye:::convertVA(va_vec8, to = "snellen", "ft"))),26) #class quali

})

test_that("va_mixed", {
  expect_equal(sum(is.na(va_mixed(mixed_VA, to = "logmar"))), 4)
  expect_equal(va_mixed(mixed_VA, to = "logmar")[3], 1.64)
  expect_equal(va_mixed(mixed_VA, to = "logmar", possible = c("logmar", "etdrs"))[3], 3)
  expect_error(va_mixed(mixed_VA), regexp = NA)

})


test_that("quali_values", {
expect_error(to_logmar(va_vec, quali_values = 2), regexp = "values need to be list")
expect_error(to_snellen(va_vec, quali_values = 2), regexp = "needs to convert to logmar")
expect_error(to_logmar(va_vec, quali_values = c(hm = 2)), regexp = "quali_values need to be list")
expect_error(to_logmar(va_vec, quali_values = c(hm = 2)), regexp = "quali_values need to be list")
expect_error(to_logmar(va_vec, quali_values = list(hm = 2, cf = 3)), regexp = "values need to be named list")
expect_error(to_logmar(va_vec, quali_values = list(hm = 2, cf = 3, npl = 4, pl = 5)), regexp = NA)
expect_error(to_logmar(va_vec, quali_values = list(hm = "3", cf = 3, npl = 4, pl = 5)), regexp = NA)
expect_error(to_logmar(va_vec, quali_values = list(hm = "20/50", cf = 3, npl = 4, pl = 5)), regexp = "quali_values need to contain only values that can be converted into numerics")
}
)


