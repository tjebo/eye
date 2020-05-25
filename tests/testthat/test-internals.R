context("internals")
library(eye)
library(testthat)

# test isNAstring
xNA <- c("6/9", ".", "   ", "", "...", "6/18", "PL", "6/4",
  "6/24", "6/36", "HM", "6/60", "EYE NOT PRESENT", "CF", "N/A",
  "6/2", "NPL", "NOT TESTED", "Not tested", "6/3", "Eye not present",
  "6/7")




