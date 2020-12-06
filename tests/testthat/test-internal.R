context("internals")
library(eye)
library(eyedata)
library(testthat)

# test isNAstring
xNA <- c("6/9", ".", "   ", "", "...", "6/18", "PL", "6/4",
         "6/24", "6/36", "HM", "6/60", "EYE NOT PRESENT", "CF", "N/A",
         "6/2", "NPL", "NOT TESTED", "Not tested", "6/3", "Eye not present",
         "6/7")

iop_true <- c('iop', 're.iop', 'iop.re', 'a.iop.re', 're_iop', 'iop_re', 'a_iop_re', 'iop2')
iop_false <- c('iopre','riop', "other", "io", 'biops', 'biop2')
va_true <- c('va', 're.va', 'va.re', 'a.va.re', 're_va', 'va_re', 'a_va_re', 'va2', 'va_l')
va_false <- c('rva', "other", "io", 'bvas', 'bva2', 'C_Lett','vare')

part_true <- c("Id", "Eye", "FolloeyepDays", "BaselidAge")
part_false <- c("I", "Ey", "FolloyepDays", "BaseliAge")

test_that("whole", {
  expect_identical(eye:::whole_str("va")(va_true), va_true)
  expect_identical(eye:::whole_str("va")(va_false), character(0))
})
test_that("partial", {
  expect_identical(eye:::part_str(c("id", "eye"))(part_true), part_true)
  expect_identical(eye:::part_str(c("id", "eye"))(part_false), character(0))
})

