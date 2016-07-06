# ISO_processing.R
#
# A script to process the ISO country codes and add the data to the Markeview package

ISO <- read_delim(file.path("M:/users/samh/Projects/Utility data/ISOcountry.txt"), delim = ".", quote = "", col_names = F)
colnames(ISO) <- c("CONTINENT", "ISO2", "COUNTRY_CODE", "No.", "COUNTRY_NAME")
ISO <- ISO[, c(1,2,3,5)]

ISO$COUNTRY_CODE <- as.character((ISO$COUNTRY_CODE))

devtools::use_data(ISO)
