context("global tests")
library(eye)
library(eyedata)
library(testthat)
#abraham <- read.csv("./data-raw/share_vision_df.csv")

test <- structure(list(VA_ETDRS_Letters = c(44, 81, 50, 40, 32, 34)), row.names = c(NA,
                                                                            -6L), class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"), spec = structure(list(
                                                                              cols = list(Id = structure(list(), class = c("collector_character",
                                                                                                                           "collector")), Eye = structure(list(), class = c("collector_double",
                                                                                                                                                                            "collector")), FollowupDays = structure(list(), class = c("collector_double",
                                                                                                                                                                                                                                      "collector")), BaselineAge = structure(list(), class = c("collector_double",
                                                                                                                                                                                                                                                                                               "collector")), Gender = structure(list(), class = c("collector_double",
                                                                                                                                                                                                                                                                                                                                                   "collector")), VA_ETDRS_Letters = structure(list(), class = c("collector_double",
                                                                                                                                                                                                                                                                                                                                                                                                                 "collector")), InjectionNumber = structure(list(), class = c("collector_double",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "collector"))), default = structure(list(), class = c("collector_guess",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "collector")), skip = 1), class = "col_spec"))
# alasdair <- read.csv("./data-raw/alasdair.csv")
# alasdair1 <- read.csv("./data-raw/alasdair1.csv")
# abraham <- read.csv("./data-raw/abraham.csv")
# # alas <- read.csv("./data-raw/alas.csv")
# # blink(amd)
# # blink(abraham)

test_that("no error", {
  #expect_error(myop(abraham), regexp = NA)
  expect_error(suppressWarnings(myop(amd)), regexp = NA)
  expect_error(suppressWarnings(blink(amd)), regexp = NA)
  #expect_error(blink(abraham), regexp = NA)
})

