# cau_projection_processing.R
#
# A script to process the Stats NZ CAU population projections and add them
# to the marketview package

# import packages
library(readr)          # improved data importing

au_projections.df <- read_csv(file.path("data-raw", "au_projections.csv"))

devtools::use_data(au_projections.df)
