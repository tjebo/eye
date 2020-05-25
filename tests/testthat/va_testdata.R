logmar <- va_chart$logMAR
snellen_ft <- va_chart$snellen_ft
snellen_m <- va_chart$snellen_m
snellen_dec <- va_chart$snellen_dec
etdrs <- va_chart$ETDRS
etdrs_unplaus <- seq(-1,30,1)
logmar_unplaus <- seq(-4.4,2.0, 0.1)
snellen_unplaus <- letters[1:20]
quali <- va_chart$quali
quali_logmar <- c(logmar, quali)
quali_snellen_ft <- c(snellen_ft, quali)
quali_snellen_m <- c(snellen_m, quali)

abraham <- read.csv("./data-raw/share_vision_df.csv")
