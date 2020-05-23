context("test myop")
library(eye)
library(testthat)

source("./tests/testthat/myop_testdata.R")

test_that("No warning",{
  plyr::l_ply(list_works, function(i) expect_warning(myop(ls_eye[[i]]), regexp = NA))
  expect_warning(myop(ls_eye[[1]], eye_code = c("re","le")), regexp = NA)
  expect_warning(myop(iop_va), regexp = NA)
  expect_warning(myop(iop_wide), regexp = NA)
})

test_that("Warning", {
  expect_warning(myop(ls_eye[[1]], eye_code = c("r","e")), "Very unusual")
  expect_warning(myop(ls_eye[[1]], eye_code = c("f","le")), "Very unusual")
  expect_warning(myop(ls_eye[[1]], va_cols = c("r","l","re")), "va_cols")
  expect_warning(myop(ls_eye[[1]], iop_cols = c("r","l","re")), "iop_cols")
})

test_that("Error",{
  plyr::l_ply(list_err, function(i) expect_error(myop(ls_eye[[i]]), "Too many eye columns - don't know how to gather"))
  plyr::l_ply(ls_eye3, function(i) expect_error(myop(i), "Too many eye columns - don't know how to gather"))
  expect_error(myop(ls_eye[[1]], eye_code = c("os","le")))
  expect_error(myop(ls_eye[[1]], eye_code = c("od","re")))
  expect_error(myop(ls_eye[[1]], eye_code = c(4:5)))
  expect_error(myop(data.frame(iop = 1)))

})

test_that("Message", {
  expect_message(myop(ls_eye[[1]], eye_code = c(0:1)), "Consider characters")
  expect_message(myop(ls_eye[[1]], eye_code = c(1:2)), "Consider characters")
  expect_message(myop(iop_va), "Gathering both VA and IOP columns")
  expect_message(myop(iop_wide), "Gathering eye columns")
  expect_message(myop(va), "Gathering VA columns")
  expect_message(myop(iop), "Gathering IOP columns")
  expect_message(myop(va_1iop), "Gathering VA columns")
  expect_message(myop(iop_1va), "Gathering IOP columns")
})

test_that("return",{
  expect_true(inherits(myop(ls_eye[[1]]), "myop"))
})

