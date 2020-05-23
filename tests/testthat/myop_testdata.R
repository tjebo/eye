set.seed(42)
id <- sample(letters[1:20])
df_sample <- as.data.frame(replicate(6, sample(30:40, size = 20, replace = T)))
eye <- c("r", "l", "re", "le", "od", "os")
colnames(df_sample) <- eye

iop_va <- data.frame(id = letters[1:11],
                     IOP_r = sample(10:20), iop_l = sample(10:20),
                     Va_r = sample(40:50), va_l = sample(40:50))
va <- data.frame(id = letters[1:11],
                 Va_r = sample(40:50), va_l = sample(40:50))
iop <- data.frame(id = letters[1:11],
                  IOP_r = sample(10:20), iop_l = sample(10:20))
va_1iop <- iop_va[-3]
iop_1va <- iop_va[-4]
iop_wide <- data.frame(id = letters[1:3], r = sample(11:13), l = sample(14:16))

ls_eye <- combn(eye, 2L, function(x) cbind(id, df_sample[, x]), simplify = FALSE)
ls_eye3 <- combn(eye, 3L, function(x) cbind(id, df_sample[, x]), simplify = FALSE)[1:5]

list_works <- c(1,3,5,6,8,10,12,13,15)
list_err <- c(2,4,7,9,11,14)
