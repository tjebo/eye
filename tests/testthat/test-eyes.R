context("test eyes")
library(eye)
library(testthat)
library(tidyverse)
source("./tests/testthat/eyes_testdata.R")

test_that("messages",{
  expect_message(eyes(foo1), regexp = "Not all eyes are identified \\(contains NA\\)")
  expect_message(eyes(foo6), "Eye coding somewhat messy")
  expect_message(eyes(foo8), "Eye coding somewhat messy")
  expect_message(eyes(foo10), "Eyes coded 0:1")
  expect_message(eyes(foo11), regexp = "Eyes coded 1:2")
  expect_message(eyes(foo17), regexp = "No eye column found")
  })

test_that("No warning",{
  expect_warning(eyes(foo2), regexp = NA)
  expect_warning(eyes(foo4), regexp = NA)
  expect_warning(eyes(foo5), regexp = NA)
  expect_warning(eyes(foo6), regexp = NA)
  expect_warning(eyes(foo8), regexp = NA)
  expect_warning(eyes(foo10), regexp = NA)
  expect_warning(eyes(foo11), regexp = NA)
})

test_that("errors",{
  expect_error(eyes(foo13), "Unclear eye coding!")
  expect_error(eyes(foo0), "Unclear eye coding!")
  expect_error(eyes(foo3), "Unclear eye coding!")
  expect_error(eyes(foo7), "Which is the")
  expect_error(eyes(foo9), "Which is the")
  expect_error(eyes(foo12), "Unclear eye coding!")
  expect_error(eyes(foo15), "Unclear eye coding!")
  expect_error(eyes(foo18), "Which is the")
  expect_error(eyes(foo19), "Unclear eye coding!")
  expect_error(eyes(foo20), "Unclear eye coding!")
  expect_error(eyes(foo21), "Unclear eye coding!")
  expect_error(eyes(foo16), "Unclear eye coding!")
  expect_error(eyes(foo14), "Unclear eye coding! ")
})

test_count <- function(x){
  eyes <- x %>% drop_na(eyes) %>% count(id, eyes) %>% nrow
  pat <- length(unique(x$id))
  return(c(pat, eyes))
}
test_count_pat <- function(x){
  eyes <- x %>% drop_na(eyes) %>% count(patient, eyes) %>% nrow
  pat <- length(unique(x$patient))
  return(c(pat, eyes))
}
test_count_idonly<- function(x){
  pat <- length(unique(x$id))
  return(c(pat))
}

test_that("return", {
  expect_equivalent(test_count(foo1), unname(eyes(foo1))[1:2])
  expect_equivalent(test_count(foo2), unname(eyes(foo2))[1:2])
  expect_equivalent(test_count(foo4), unname(eyes(foo4))[1:2])
  expect_equivalent(test_count(foo10), unname(eyes(foo10))[1:2])
  expect_equivalent(test_count(foo11), unname(eyes(foo11))[1:2])
  expect_equivalent(test_count_pat(foo5), unname(eyes(foo5))[1:2])
  expect_equivalent(test_count_pat(foo6), unname(eyes(foo6))[1:2])
  expect_equivalent(test_count_pat(foo8), unname(eyes(foo8))[1:2])
  expect_equivalent(test_count_idonly(foo17), unname(eyes(foo17))[1])
})




