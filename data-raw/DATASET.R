library(dplyr)

## AMD dataset
amd_raw <- readr::read_delim("./data-raw/Moorfields_AMD_Database_1.csv", delim = ";")
amd_raw$Id <- paste0("id_", as.integer(as.factor(amd_raw$Id)))
amd <-
  amd_raw %>%
  rename(patID = Id, eye = Eye, va = VA_ETDRS_Letters,
         inj_no = InjectionNumber, time = FollowupDays,
         sex = Gender, age0 = BaselineAge) %>%
  mutate(eye = recodeye(eye),
         sex = if_else(sex == 0, "m", "f")) %>%
  select(patID, sex, age0, everything())

usethis::use_data(amd, overwrite = TRUE)

### DME data
dme_raw <- read.csv("./data-raw/200319_DMO_report1_anonymised.csv")
## simplify patient code
dme_raw$anon_id <- paste0("id_", as.integer(as.factor(dme_raw$anon_id)))
dme_raw[["inj_num"]] <- NULL
## replacing implausible ETDRS values with NA
dme_raw$va[dme_raw$va>100] <- NA
lu_eth <- c("asian", "unknown", "other",  "white", "black", "mixed")
names(lu_eth) <- unique(dme_raw$ethnicity)

dme <-
  dme_raw %>%
  select(patID = anon_id, sex = gender, ageStrat = baseline_age, ethnicity,
         everything(), -X, -baseline_va) %>%
  rename(inj = inj_given, time = follow_up_days) %>%
  mutate(inj = inj == "y")

sort_id <- paste0("id_", seq_along(unique(dme$patID)))

dme <- dme %>%
  mutate(patID = factor(patID, levels = sort_id)) %>%
  arrange(patID, eye, time) %>%
  mutate(patID = as.character(patID))
dme$ethnicity <- lu_eth[dme$ethnicity]
dme <- tidyr::as_tibble(dme)
usethis::use_data(dme, overwrite = TRUE)
###  va conversion chart
#Snellen converted to logmar = -1 * log10(Snellen fraction).
snellen_ft <-
  c( "20/800", "20/630", "20/500", "20/400", "20/320", "20/300", "20/250", "20/200",
    "20/160", "20/125","20/120","20/100","20/80","20/70", "20/63",
    "20/60", "20/50","20/40","20/32","20/30","20/25","20/20",
    "20/16","20/15", "20/13", "20/10")

snellen_m <- c( "6/240","6/190","6/150", "6/120", "6/96","6/90", "6/75", "6/60", "6/48",
                "6/38","6/36", "6/30", "6/24", "6/21", "6/19", "6/18", "6/15", "6/12", "6/9.6",
               "6/9", "6/7.5", "6/6", "6/5", "6/4.5", "6/4", "6/3")
dec_sn <- with(read.table(text = snellen_ft[!is.na(snellen_ft)], sep = "/"), V1 / V2)
logmar <- round(-1 * log10(dec_sn[!is.na(dec_sn)]), 2)
etdrs <- round(85 + 50 * log10(dec_sn), 0)


va_chart1 <- data.frame(snellen_ft,
                             snellen_m,
                            snellen_dec = formatC(round(dec_sn, 3)),
                            logmar = formatC(logmar),
                            etdrs = as.integer(etdrs),
                       quali = rep(NA, length(logmar)))
quali <- data.frame(snellen_ft = c("20/20000", "20/10000", "20/4000", "20/2000"),
                    snellen_m = c("6/6000", "6/3000", "6/1200", "6/600"),
                    snellen_dec = c(0.001, 0.002, 0.005, 0.01),
                    logmar = c(3.0, 2.7, 2.3, 1.9),
                    etdrs = c(0, 0, 0, 2),
                    quali = c("NLP", "LP", "HM", "CF"))

va_chart <- rbind(quali, va_chart1)
va_chart$etdrs[va_chart$etdrs %in% 94] <- 95L
logmar_replace <- c("1", "-0", "-0.19")
va_chart$logmar[va_chart$logmar %in% logmar_replace] <- c("1.0", "0.0", "-0.2")
snell_dec_replace <- c("0.286", "0.317", "0.333", "0.667", "1", "1.333", "1.538", "2")
va_chart$snellen_dec[va_chart$snellen_dec %in% snell_dec_replace] <-
  c("0.3", '0.32', '0.33', '0.66', '1.0', '1.33', '1.5', '2.0')
va_chart$logmar <-  round(as.numeric(va_chart$logmar), 2)
va_chart$etdrs <-  as.integer(va_chart$etdrs)

usethis::use_data(va_chart, overwrite = TRUE)


va_quali <- va_chart[!is.na(va_chart$quali), ]
va_quali$quali <- tolower(va_quali$quali )
names(va_quali) <- tolower(names(va_quali))
va_quali$logmar <- round(as.numeric(va_quali$logmar),1)
va_quali$etdrs <- as.integer(c(0, 0, 0, 2))
usethis::use_data(va_quali, internal = TRUE, overwrite = TRUE)
