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

va_vec <- c("NLP", "LP", "HM", "CF", "6/60", "20/200", "6/9", "20/40")
va_vec2 <- structure(c(3, 2.7, 2.3, 1.9, 1, 1, 0.18, 0.3), class = c("logmar",
                                                                     "va", "numeric"))
va_vec3 <- structure(c(0L, 0L, 0L, 2L, 35L, 35L, 75L, 70L), class = c("etdrs",
                                                                      "va", "integer"))

abraham <- read.csv("./data-raw/share_vision_df.csv")

