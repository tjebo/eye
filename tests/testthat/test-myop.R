context("test myop")
library(eye)
library(testthat)

source("./tests/testthat/myop_testdata.R")

test_that("No warning",{
  expect_warning(myop(df_iopva), regexp = NA)
  expect_warning(myop(df_novarname), regexp = NA)
  expect_warning(myop(df_work), regexp = NA)
  expect_warning(myop(df_noid), regexp = NA)
  expect_warning(myop(df_onevarfail), regexp = NA)
})

test_that("warning", {
  expect_warning(myop(amd), "Can't make this myopic")
  expect_warning(myop(build_df), "Can't make this myopic")
  expect_warning(myop(fail_dup), "Removed duplicate rows")
})

duplicated(fail_dup)

ls_testdf <- mget(ls(pattern = "df_"))
len_testdf <- sapply(ls_testdf, nrow)
len_resmyop <- sapply(ls_testdf, function(x) nrow(myop(x)))

test_that("return",{
  expect_equal(len_resmyop, 2*len_testdf)
})
