set.seed(42)
id <- sample(letters[1:10])
build_df <- cbind(id, as.data.frame(replicate(9, sample(30:40, size = 10, replace = T))))
df_work<- setNames(build_df, c("id","od","os", "va_r", "va_l", "post_va_r", "right.morningpressure", "left_morningpressure", "night_iop.le", "gat_os_postop"))

df_noid <- df_work[-1]
df_onevarfail <- setNames(build_df[-2][-2], c("id","var", "va_l", "post_va_r", "right.morningpressure", "left_morningpressure", "night_iop.le", "gat_os_postop"))

df_iopva <- setNames(build_df[1:5], c("id", outer(c("va","iop"), c("r","l"), paste, sep = "_")))

fail_dup <- rbind(df_iopva, head(df_iopva,2))

df_novarname <- data.frame(id = letters[1:3], r = sample(11:13), l = sample(14:16))


