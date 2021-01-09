library(dplyr)
library(eye)

###  va conversion chart
#Snellen converted to logmar = -1 * log10(Snellen fraction).
snellenft <-
  c( "20/800", "20/630", "20/500", "20/400", "20/320", "20/300", "20/250", "20/200",
    "20/160", "20/125","20/120","20/100","20/80","20/70", "20/63",
    "20/60", "20/50","20/40","20/32","20/30","20/25","20/20",
    "20/16","20/15", "20/13", "20/10")

snellenm <- c( "6/240","6/190","6/150", "6/120", "6/96","6/90", "6/75", "6/60", "6/48",
                "6/38","6/36", "6/30", "6/24", "6/21", "6/19", "6/18", "6/15", "6/12", "6/9.6",
               "6/9", "6/7.5", "6/6", "6/5", "6/4.5", "6/4", "6/3")
dec_sn <- with(read.table(text = snellenft[!is.na(snellenft)], sep = "/"), V1 / V2)
logmar <- round(-1 * log10(dec_sn[!is.na(dec_sn)]), 2)
etdrs <- round(85 + 50 * log10(dec_sn), 0)


va_chart1 <- data.frame(snellenft,
                             snellenm,
                            snellendec = formatC(round(dec_sn, 3)),
                            logmar = formatC(logmar),
                            etdrs = as.integer(etdrs),
                       quali = rep(NA, length(logmar)))
quali <- data.frame(snellenft = c("20/20000", "20/10000", "20/4000", "20/2000"),
                    snellenm = c("6/6000", "6/3000", "6/1200", "6/600"),
                    snellendec = c(0.001, 0.002, 0.005, 0.01),
                    logmar = c(3.0, 2.7, 2.3, 1.9),
                    etdrs = c(-222, -111, 0, 2),
                    quali = c("nlp", "lp", "hm", "cf"))

va_chart <- rbind(quali, va_chart1)
va_chart$etdrs[va_chart$etdrs %in% 94] <- 95L
logmar_replace <- c("1", "-0", "-0.19")
va_chart$logmar[va_chart$logmar %in% logmar_replace] <- c("1.0", "0.0", "-0.2")
snell_dec_replace <- c("0.286", "0.317", "0.333", "0.667", "1", "1.333", "1.538", "2")
va_chart$snellendec[va_chart$snellendec %in% snell_dec_replace] <-
  c("0.3", '0.32', '0.33', '0.66', '1.0', '1.33', '1.5', '2.0')
va_chart$logmar <-  round(as.numeric(va_chart$logmar), 2)
va_chart$etdrs <-  as.integer(va_chart$etdrs)

usethis::use_data(va_chart, overwrite = TRUE, internal = TRUE)
