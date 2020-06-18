context("test va")
library(eye)
library(testthat)


logmar <- va_chart$logmar
snellen_ft <- va_chart$snellen_ft
snellen_m <- va_chart$snellen_m
snellen_dec <- va_chart$snellen_dec
etdrs <- va_chart$etdrs
etdrs_unplaus <- seq(-1,30,1)
logmar_unplaus <- seq(-4.4,2.0, 0.1)
snellen_unplaus <- letters[1:20]
quali <- va_chart$quali
quali_logmar <- c(logmar, quali)
quali_etdrs<- c(etdrs, quali)
quali_snellen_ft <- c(snellen_ft, quali)
quali_snellen_m <- c(snellen_m, quali)
mixed_VA <- c(32, "20/40",3, NA, "1.1", 0.2, -0.3, "NA", "eye", -0.4, "NLP", "PL")
mixed_VA1 <- c(32, "20/40",3, NA, "1.1", 0.2, -0.3, "NA",  -0.4, "NLP", "PL")
mixed_VA2 <- c(32, "20/40",3, NA, "1.1", 0.2, -0.3, "NA",   "NLP", "PL")
etdrs_logmar <- 0:1
etdrs_logmar_na <- c(0:1, NA)
snellen_logmar <- intersect(as.numeric(va_chart$snellen_dec), va_chart$logmar)
snellen_logmar_na <- c(snellen_logmar, NA)
va_vecNA <- c(NA, NA)
va_vec <- c("NLP", "LP", "HM", "CF", "6/60", "NA", NA, "20/40")
va_vec1 <- c("NPL", "NLP", "LP", "PL", "HM", "CF", "6/60", "20/200", "6/9", "20/40")
va_vec2 <- structure(c(3, 2.7, 2.3, 1.9, 1, 1, 0.18, 0.3), class = c("logmar",
                                                                     "va", "numeric"))
va_vec3 <- structure(c(0L, 0L, 0L, 2L, 35L, 35L, 75L, 70L), class = c("etdrs",
                                                                      "va", "integer"))
va_vec4 <- c(NA, "CF", "NA", "HM", "6/9", "6/6", "6/18", "6/12", "6/5",
             "6/24", "6/36", "6/60", "3/60")
va_vec5 <- c("20/200","20/200 + 3", "20/200+3", "20/200-4","20/200 -4")

va_vec6 <- c("6/9", "6/6", "6/5", "6/12", "6/7.5", "6/18", "PL", "6/4",
             "6/24", "6/36", "HM", "6/60", "EYE NOT PRESENT", "CF", "N/A",
             "6/2", "NPL", "NOT TESTED", "Not tested", "6/3", "Eye not present",
             "6/7")
va_vec7 <- structure(c(NA, "6/9", "6/6", "6/18", "6/12", "6/5",
             "6/24", "6/36", "6/60", "3/60" ),class = c("snellen",
                                                      "va", "integer"))
va_vec8 <- structure(quali,class = c("quali", "va", "integer"))


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
})

test_that("no error / no warning", {

  expect_error(va(logmar, to = "etdrs"), regexp = NA)
  expect_error(va(etdrs), regexp = NA)
  expect_error(va(snellen_dec), regexp = NA)
  expect_error(va(logmar, to = "logmar"), regexp = NA)
  expect_warning(va(logmar, to = "logmar"), regexp = NA)
  expect_error(va(snellen_m), regexp = NA)
  expect_error(va(snellen_ft), regexp = NA)
  expect_warning(va(quali_logmar, to = "etdrs"), regexp = NA)
  expect_warning(va(quali_snellen_ft, to = "etdrs"), regexp = NA)
  expect_warning(va(va_vec), regexp = NA)
  expect_warning(va(va_vec2, to = "snellen"), regexp = NA)
  expect_warning(va(va_vec3), regexp = NA)

})

test_that("message", {
  expect_message(va(mixed_VA), "Mixed object")
  expect_message(va(mixed_VA1), "Mixed object")
  expect_message(va(mixed_VA2), "Mixed object")
  expect_message(va(etdrs), regexp = "from etdrs")
  expect_message(va(snellen_dec, from_logmar = FALSE), "from snellen")
  expect_message(va(quali_snellen_ft), "from snellen")
  expect_message(va(logmar), "from logmar")
  expect_message(va(etdrs_logmar_na), "Notation ambiguous - logMAR picked.")
  expect_message(va(snellen_logmar), "Notation ambiguous - logMAR picked.")
  expect_message(va(snellen_logmar_na), "Notation ambiguous - logMAR picked.")
  expect_message(va(etdrs_logmar), "Notation ambiguous - logMAR picked.")
})

test_that("warning", {
  expect_warning(va(mixed_VA), "NA introduced")
  expect_warning(va(mixed_VA1), "NA introduced")

  expect_warning(va(snellen_unplaus), "No conversion")
  expect_warning(va(amd$VA_ETDRS_Letters), "implausible values")
  expect_warning(va(etdrs_unplaus), "implausible values")
  expect_warning(va(logmar_unplaus), "implausible values")
  expect_warning(va(logmar, to = "snellen", type = "random"), "Ignoring \"type", fixed = TRUE)
  expect_warning(va(mixed_VA), "implausible values")
  expect_warning(va(mixed_VA1), "NA introduced")

})

test_that("return", {
  expect_identical(eye:::which_va(etdrs), "etdrs")
  expect_identical(eye:::which_va(snellen_dec), c("logmar","snellen"))
  expect_identical(eye:::which_va(snellen_m), "snellen")
  expect_identical(eye:::which_va(snellen_ft), "snellen")
  expect_identical(eye:::which_va(logmar), "logmar")
  expect_identical(eye:::which_va(clean_va(quali)), "quali")
  expect_identical(eye:::which_va(etdrs_unplaus), c("etdrs", "implaus"))
  expect_identical(eye:::which_va(logmar_unplaus), c("logmar", "implaus"))
  expect_identical(eye:::which_va(snellen_unplaus), "failed")
  expect_identical(eye:::which_va(clean_va(quali_logmar)), "logmar")
  expect_identical(eye:::which_va(clean_va(quali_snellen_ft)), "snellen")
  expect_identical(eye:::which_va(clean_va(quali_snellen_m)), "snellen")
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
  expect_true(all(grepl("20/", va(logmar, to = "snellen", type = "random"))))
  expect_true(all(grepl("6/", va(logmar, to = "snellen", type = "m"))))
  expect_true(all(va(logmar, to = "snellen", type = "dec") %in% va_chart$snellen_dec))
  expect_true(inherits(va(mixed_VA), "logmar"))
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

test_that("NA", {
  expect_equal(sum(is.na(va(va_vec))), 2)
  expect_equal(sum(is.na(va(va_vecNA))), 2)
  expect_equal(sum(is.na(va(va_vec1))), 0)
  expect_equal(sum(is.na(va(va_vec2))), 0)
  expect_equal(sum(is.na(va(va_vec3))), 0)
  expect_equal(sum(is.na(va(va_vec4))), 2)
  expect_equal(sum(is.na(va(va_vec5))), 0)
  expect_equal(sum(is.na(va(va_vec6))), 5)
  expect_equal(sum(is.na(eye:::convertVA(va_vec2, to = "snellen", "ft"))),0) #class logmar
  expect_equal(sum(is.na(eye:::convertVA(va_vec2, to = "etdrs", "ft"))),0) #class logmar
  expect_equal(sum(is.na(eye:::convertVA(va_vec2, to = "logmar", "ft"))),0) #class logmar
  expect_equal(sum(is.na(eye:::convertVA(va_vec3, to = "etdrs", "ft"))),0) #class etdrs
  expect_equal(sum(is.na(eye:::convertVA(va_vec3, to = "snellen", "ft"))),0) #class etdrs
  expect_equal(sum(is.na(eye:::convertVA(va_vec3, to = "logmar", "ft"))),0) #class etdrs
  expect_equal(sum(is.na(eye:::convertVA(va_vec7, logmarstep = FALSE, to = "snellen", "dec")))-1,1) #class snellen
  expect_equal(sum(is.na(eye:::convertVA(va_vec7,  logmarstep = FALSE,to = "etdrs", "ft"))),1) #class snellen
  expect_equal(sum(is.na(eye:::convertVA(va_vec7, logmarstep = FALSE, to = "logmar", "ft"))),1) #class snellen
  expect_equal(sum(is.na(eye:::convertVA(va_vec8, to = "snellen", "ft"))),26) #class quali
  expect_equal(sum(is.na(eye:::convertVA(va_vec8, to = "etdrs", "ft"))),26) #class quali
  expect_equal(sum(is.na(eye:::convertVA(va_vec8, to = "logmar", "ft"))),26) #class quali
  expect_equal(sum(is.na(va(c(25, 23, 0.4), to = "snellen", from_logmar = FALSE))),1) #class quali
  expect_equal(sum(is.na(va(c(25, 23, 0.4), to = "snellen"))), 2) #class quali
  expect_equal(sum(is.na(va(mixed_VA))), 4)
  expect_equal(sum(is.na(va(mixed_VA1))), 3)
  expect_equal(sum(is.na(va(mixed_VA2))), 2)
  }
)


