## code to prepare `DATASET` dataset goes here
amd <- readr::read_delim("data-raw/Moorfields_AMD_Database_1.csv", delim = ";")

usethis::use_data(amd, overwrite = TRUE)


#S nellen converted to logMAR = -1 * log(Snellen fraction).
snellen <-
  c("1/200","2/200","20/800","6/200","20/640", "20/500",
    "20/400", "20/320", "20/300","20/252","20/250", "20/200",
    "20/160", "20/125","20/100","20/80","20/70", "20/63",
    "20/60", "20/50","20/40","20/32","20/30","20/25","20/20",
    "20/16","20/15", "20/13", "20/10")

dec_sn <- with(read.table(text = snellen, sep = "/"), V1 / V2)
logMAR <- round(-1 * log10(dec_sn), 2)
ETDRS <- round(85 + 50 * log10(dec_sn), 0)
va_quali_vec <- c("LP" = 4.2, "HM" = 2.9, "CF" = 2.3)
va_quali <- tibble::enframe(va_quali_vec)
names(va_quali) <- c("quali", "logMAR")

va_conversion <- data.frame(snellen = factor(snellen, levels = snellen),
                            logMAR,
                            ETDRS)
va_conversion$ETDRS[1:2] <- c(0,2)

usethis::use_data(va_conversion, va_quali, internal = TRUE, overwrite = TRUE)

