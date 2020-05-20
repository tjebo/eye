context("test myop")
library(eye)
library(testthat)

set.seed(42)
id <- sample(letters[1:20])
df_sample <- as.data.frame(replicate(6, sample(30:40, size = 20, replace = T)))
eye <- c("r", "l", "re", "le", "od", "os")
colnames(df_sample) <- eye

iop_va <- data.frame(id = letters[1:11],
                     IOP_r = sample(10:20), iop_l = sample(10:20),
                     Va_r = sample(40:50), va_l = sample(40:50))

iop_wide <- data.frame(id = letters[1:3], r = sample(11:13), l = sample(14:16))

ls_eye <- combn(eye, 2L, function(x) cbind(id, df_sample[, x]), simplify = FALSE)
ls_eye3 <- combn(eye, 3L, function(x) cbind(id, df_sample[, x]), simplify = FALSE)[1:5]

list_works <- c(1,3,5,6,8,10,12,13,15)
list_err <- c(2,4,7,9,11,14)

test_that("No warning",{
  plyr::l_ply(list_works, function(i) expect_warning(myop(ls_eye[[i]]), regexp = NA))
  expect_warning(myop(ls_eye[[1]], eye_code = c("re","le")), regexp = NA)
  expect_warning(myop(iop_va), regexp = NA)
  expect_warning(myop(iop_wide), regexp = NA)
})

test_that("Warning", {
  expect_warning(myop(ls_eye[[1]], eye_code = c("r","e")), "Very unusual")
  expect_warning(myop(ls_eye[[1]], eye_code = c("f","le")), "Very unusual")
})

test_that("Error",{
  plyr::l_ply(list_err, function(i) expect_error(myop(ls_eye[[i]]), "Eye columns ambiguous."))
  plyr::l_ply(ls_eye3, function(i) expect_error(myop(i), "Eye columns ambiguous."))
  expect_error(myop(ls_eye[[1]], eye_code = c("le","le")))
  expect_error(myop(ls_eye[[1]], eye_code = c("os","le")))
  expect_error(myop(ls_eye[[1]], eye_code = c("od","re")))
  expect_error(myop(ls_eye[[1]], eye_code = c(4:5)))
  expect_error(myop(ls_eye[[1]], value = letters[1:2]), "Only one")
  expect_error(myop(ls_eye[[1]], cols = c("r","l","re")), "Two variables")

})

test_that("Message", {
  expect_message(myop(ls_eye[[1]], eye_code = c(0:1)), "Consider characters")
  expect_message(myop(ls_eye[[1]], eye_code = c(1:2)), "Consider characters")
  expect_message(myop(iop_va))
  expect_message(myop(iop_wide))
})



