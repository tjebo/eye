context("reveal")
library(eye)
library(testthat)

set.seed(42)
x <- y <- z <- c(rnorm(11), NA)
mydf <- data.frame(group = rep(letters[1:3], 4), x, y, z)
mylist <- list(x = x, y = y, z = z)

# reveal(1:10, funs = list(mean))
# reveal(mylist, funs = list(a = function(x) mean(x, na.rm = TRUE)))
# reveal(mydf)
# reveal(mydf, by = "group")

 test_that("no error",{
   expect_error(reveal(x), regexp = NA)
   expect_error(reveal(1:10), regexp = NA)
   expect_error(reveal(mydf), regexp = NA)
   expect_error(reveal(mydf, by = "group"), regexp = NA)
   expect_error(reveal(mylist), regexp = NA)
 })

 test_that("no warning",{
   expect_warning(reveal(mydf, by = "group"), regexp = NA)
   expect_warning(reveal(x), regexp = NA)
   expect_warning(reveal(1:10), regexp = NA)
   expect_warning(reveal(mylist), regexp = NA)
 })
 test_that("warning",{
   expect_warning(reveal(mydf), "character elements")
 })

