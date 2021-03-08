context("test va")
library(eye)
library(testthat)

xtry <- c(NA, "nlp", 1:2, 1.1, -1, "20/40", "4/6", "6/1000", 34)
logmar <- va_chart$logmar
snellenft <- va_chart$snellenft
snellenm <- va_chart$snellenm
snellendec <- va_chart$snellendec
etdrs <- va_chart$etdrs
etdrs_unplaus <- seq(-1,30,1)
logmar_unplaus <- seq(-4.4,2.0, 0.1)
snellen_unplaus <- letters[1:20]
quali <- va_chart$quali
quali_logmar <- c(logmar, quali)
quali_etdrs<- c(etdrs, quali)
quali_snellen_ft <- c(snellenft, quali)
quali_snellen_m <- c(snellenm, quali)
mixed_VA <- c(32, "20/40",3, NA, "1.1", 0.2, -0.3, "NA", "eye", -0.4, "NLP", "PL")
mixed_VA1 <- c(32, "20/40",3, NA, "1.1", 0.2, -0.3, "NA",  -0.4, "NLP", "PL")
mixed_VA2 <- c(32, "20/40",3, NA, "1.1", 0.2, -0.3, "NA",   "NLP", "PL")
etdrs_logmar <- 0:1
etdrs_logmar2 <-  c(0.2, 0.1, 75)
etdrs_logmar_na <- c(0:1, NA)
snellen_logmar <- intersect(as.numeric(va_chart$snellendec), va_chart$logmar)
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
                                                      "va", "character"))
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

test_that("NA", {
  expect_equal(sum(is.na(va(va_vec))), 2)
  expect_equal(suppressWarnings(sum(is.na(va(va_vecNA)))), 2)
  expect_equal(sum(is.na(va(va_vec1))), 0)
  expect_equal(sum(is.na(va(va_vec2))), 0)
  expect_equal(sum(is.na(va(va_vec3))), 0)
  expect_equal(sum(is.na(va(va_vec4))), 2)
  expect_equal(sum(is.na(va(va_vec5))), 0)
  expect_equal(sum(is.na(va(va_vec6))), 5)
  expect_equal(sum(is.na(va(va_vec8, to = "etdrs"))), 26)
  expect_equal(sum(is.na(va(va_vec8, to = "snellen", type = "dec"))), 26)
  expect_equal(sum(is.na(eye:::convertVA(va_vec2, to = "snellen", "ft"))),0) #class logmar
  expect_equal(sum(is.na(eye:::convertVA(va_vec2, to = "logmar", "ft"))),0) #class logmar
  expect_equal(sum(is.na(eye:::convertVA(va_vec3, to = "etdrs", "ft"))),0) #class etdrs
  expect_equal(sum(is.na(eye:::convertVA(va_vec3, to = "snellen", "ft"))),0) #class etdrs
  expect_equal(sum(is.na(eye:::convertVA(va_vec3, to = "logmar", "ft"))),0) #class etdrs
  expect_equal(sum(is.na(eye:::convertVA(va_vec7, smallstep = FALSE, noplus = FALSE, to = "snellen", "dec"))), 1) #class snellen
  expect_equal(sum(is.na(eye:::convertVA(va_vec7,  smallstep = FALSE,noplus = FALSE,to = "etdrs", "ft"))),1) #class snellen
  expect_equal(sum(is.na(eye:::convertVA(va_vec7, smallstep = FALSE, noplus = FALSE,to = "logmar", "ft"))),1) #class snellen
  expect_equal(sum(is.na(eye:::convertVA(va_vec8, to = "snellen", "ft"))),26) #class quali
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






