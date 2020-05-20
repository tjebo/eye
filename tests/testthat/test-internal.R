context("test internals")
library(eye)
library(testthat)

library(testthat)
iop_true <- c('iop', 're.iop', 'iop.re', 'a.iop.re', 're_iop', 'iop_re', 'a_iop_re', 'iop2')
iop_false <- c('iopre','riop', "other", "io", 'biops', 'biop2')
va_true <- c('va', 're.va', 'va.re', 'a.va.re', 're_va', 'va_re', 'a_va_re', 'va2', 'va_l')
va_false <- c('rva', "other", "io", 'bvas', 'bva2', 'C_Lett','vare')

part_true <- c("Id", "Eye", "FolloeyepDays", "BaselidAge")
part_false <- c("I", "Ey", "FolloyepDays", "BaseliAge")

whole("va")(va_true)

test_that("whole", {
  expect_identical(whole("va")(va_true), va_true)
  expect_identical(whole("va")(va_false), character(0))
})
test_that("partial", {
  expect_identical(partial(c("id", "eye"))(part_true), part_true)
  expect_identical(partial(c("id", "eye"))(part_false), character(0))
})

