## code to prepare `DATASET` dataset goes here
amd <- readr::read_delim("data-raw/Moorfields_AMD_Database_1.csv", delim = ";")

usethis::use_data(amd, overwrite = TRUE)
