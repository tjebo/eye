context("test eyes")
library(eye)
library(eyedata)
library(testthat)

iop_wide <- data.frame(id = letters[1:3], iop_r = 11:13, iop_l = 14:16)

set.seed(42)
patient <- id <- c(letters[sample(20)], letters[sample(20)])
foo0 <- data.frame(id, eyes = c(1:4))
foo1 <- data.frame(id, eyes = c(NA, "l"))
foo2 <- data.frame(id, eyes = c("r", "l"))
foo3 <- data.frame(id , eyes = c("e", "l"))
foo4 <- data.frame(id, eyes = c("od", "le"))
foo5 <- data.frame(patient , eyes = c("od", "le"))
foo6 <- data.frame(patient, eyes = c("od", "le", "re", "le"))
foo7 <- data.frame(patient, eyes = c("od", "le"), patience = "c")
foo8 <- data.frame(patient, eyes = c("od", "le", "re", "os"))
foo9 <- data.frame(patient, eyes = c("od", "le", "re", "os"), eye = "d")
foo10 <- data.frame(id, eyes = c(1:0))
foo11 <- data.frame(id, eyes = c(1:2))
foo12 <- data.frame(id, eyes = c(1, "l"))
foo13 <- data.frame(id, eyes = c(1, 2,"r", "r"))
foo14 <- data.frame(id, eyes = c(1, 2,"r", "l"))
foo15 <- data.frame(id, eyes = c(0, 2,"r", "l"))
foo16 <- data.frame(id, eyes = c(0, 1,"l", "l"))
foo17 <- data.frame(id)
foo18 <- data.frame(eyes = c(0, 1,"l", "l"))
foo19 <- data.frame(id, eyes = c(1,3))
foo20 <- data.frame(id , eyes = c("e", "f"))
foo21 <- data.frame(id , eyes = c("r", "f"))

# sample data
id <- letters[sample(11, replace = TRUE)]
foo31 <- data.frame(id, eye = sample(c("r", "l", "b"), 11, replace = TRUE))
foo32 <- data.frame(id, eye = "r")
foo33 <- foo2
foo33$eye[1:5] <- NA
foo34 <- data.frame(id, eye = "b")


test_that("messages",{
  expect_message(eyes(foo1), regexp = "Missing values and")
  # expect_message(eyes(foo6), "Eye coding somewhat messy")
  # expect_message(eyes(foo8), "Eye coding somewhat messy")
  expect_message(eyes(foo10), "Eyes coded 0:1")
  expect_message(eyes(foo11), regexp = "Eyes coded 1:2")
  expect_message(eyes(foo17), regexp = "Unclear which is the eye column")
  expect_message(eyes(foo9), regexp = "Unclear which is the eye column")
  expect_message(eyes(foo34), regexp = "Some rows")
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

test_that("warning/warning",{
  expect_message(eyes(foo0), "NOT RECODED")
  expect_message(eyes(foo3), "Introduced NA for unclear values")
  expect_warning(eyes(foo7), "Did not find")
  expect_warning(eyes(foo18), "Did not find")
  expect_message(eyes(foo19), "NOT RECODED")
  expect_message(eyes(foo14), "Introduced NA for unclear values")
  expect_message(eyes(foo0), "NOT RECODED")
  expect_message(eyes(foo19), "NOT RECODED")
  expect_message(eyes(foo20), "Introduced NA for unclear values")
  expect_message(eyes(foo21), "Introduced NA for unclear values")
  expect_message(eyes(foo16), "Introduced NA for unclear values")
  expect_message(eyes(foo13), "Introduced NA for unclear values")
  expect_message(eyes(foo12), "Introduced NA for unclear values")
  expect_message(eyes(foo15), "Introduced NA for unclear values")
})

test_that("NULL", {
expect_null(suppressWarnings(eyes(foo7)))
expect_null(suppressWarnings(eyes(foo18)))
}
)

test_that("return", {
  expect_true(length(eyestr(iop_wide)) == 1)
  expect_true(length(suppressWarnings(eyes(foo3, dropunknown = FALSE))) == 1)
})
eyes(iop_wide)
