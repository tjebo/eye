context("test myop")
library(eye)
library(testthat)

source("./tests/testthat/myop_testdata.R")


test_that("No warning",{
  plyr::l_ply(list_works, function(i) expect_warning(myop(ls_eye[[i]]), regexp = NA))
  expect_warning(myop(ls_eye[[1]]), regexp = NA)
  expect_warning(myop(iop_va), regexp = NA)
  expect_warning(myop(iop_wide), regexp = NA)
})

test_that("Warning", {
})

test_that("Error",{
  plyr::l_ply(list_err, function(i) expect_error(myop(ls_eye[[i]]), "Many eye columns and no VA/IO"))
  plyr::l_ply(ls_eye3, function(i) expect_error(myop(i), "Many eye columns and no VA/IO"))
  expect_error(myop(data.frame(iop = 1)), "No columns found")
  expect_error(myop(amd), "No columns found")
  expect_error(myop(abraham), "cannot do this yet")

})

test_that("Message", {
  expect_message(myop(iop_va), "Gathering both VA and IOP columns")
  expect_message(myop(ls_eye[[1]]), "Gathering eye columns")
  expect_message(myop(iop_wide), "Gathering eye columns")
  expect_message(myop(va), "Gathering VA columns")
  expect_message(myop(iop), "Gathering IOP columns")
  expect_message(myop(va_1iop), "Gathering VA columns")
  expect_message(myop(iop_1va), "Gathering IOP columns")
})

test_that("return",{
  expect_true(inherits(myop(ls_eye[[1]]), "myop"))
})

