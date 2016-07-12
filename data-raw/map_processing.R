# map_processing.R
#
# A script to process the NZ map data and add it to the /data directory of
# the Marketview package

# import packages
library(magrittr)
library(dplyr)          # data manipulation
library(readr)          # improved data importing
library(ggplot2)
library(grid)
library(gridExtra)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(spatstat)


# NZ Highways -------------------------------------------------------------

nz_highways.sldf <- readOGR(dsn = "M:/gisdata/misc", layer = "highways", stringsAsFactors = FALSE)
nz_highways.sldf$id <- rownames(nz_highways.sldf@data)

highways.df <- nz_highways.sldf %>%
  fortify(region = "id") %>%
  left_join(nz_highways.sldf@data)

devtools::use_data(highways.df)

# 2013 AREA UNITS ---------------------------------------------------------

# directory for the map data
gis_dir <- "M:/gisdata/2013 Boundaries/ESRI shapefile Output/2013 Digital Boundaries Generlised Full"

# import the full area unit shapefile
nz_map.spdf <- readOGR(dsn = gis_dir, layer = "AU2013_GV_Full", stringsAsFactors = FALSE)
nz_map.spdf$id <- rownames(nz_map.spdf@data)

# import the data from the meshblock shapefile
mb_info.df <- readOGR(dsn = gis_dir, layer = "MB2013_GV_Full", stringsAsFactors = FALSE)@data

nz_cau_13.spdf <- nz_map.spdf
devtools::use_data(nz_cau_13.spdf)

# Auckland -----------------------------------------------------

akl_meshblocks.df <- mb_info.df %>%
  filter(as.integer(TA2013) %in% c(76))

# prepare the maps
akl_cau_13.df <- nz_map.spdf[(nz_map.spdf@data$AU2013 %in% akl_meshblocks.df$AU2013) & !(grepl("Estuary|Marinas|[iI]nlet|[Tt]idal|^[Hh]arbour|[Oo]ceanic|Tamaki Strait|^Bays(?!water)|Water", x = nz_map.spdf@data$AU2013_NAM, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  mutate(AU2013 = as.integer(AU2013)) %>%
  select(long, lat, group, CAU = AU2013, CAU_NAME = AU2013_NAM)

devtools::use_data(akl_cau_13.df)



# Wellington ---------------------------------------------------

wgtn_meshblocks.df <- mb_info.df %>%
  filter(as.integer(TA2013) %in% c(43:47, 50))

# prepare the maps
wtn_cau_13.df <- nz_map.spdf[(nz_map.spdf@data$AU2013 %in% wgtn_meshblocks.df$AU2013) & !(grepl("Estuary|Marinas|[iI]nlet|[Tt]idal|^[Hh]arbour|[Oo]ceanic|Tamaki Strait|^Bays(?!water)|Water", x = nz_map.spdf@data$AU2013_NAM, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  mutate(AU2013 = as.integer(AU2013)) %>%
  select(long, lat, group, CAU = AU2013, CAU_NAME = AU2013_NAM)

devtools::use_data(wtn_cau_13.df)

# Christchurch -------------------------------------------------

chch_meshblocks.df <- mb_info.df %>%
  filter(as.integer(TA2013) %in% c(60, 59, 62))

# prepare the maps
chch_cau_13.df <- nz_map.spdf[(nz_map.spdf@data$AU2013 %in% chch_meshblocks.df$AU2013) & !(grepl("Estuary|Marinas|[iI]nlet|[Tt]idal|^[Hh]arbour|[Oo]ceanic|Tamaki Strait|^Bays(?!water)|Water", x = nz_map.spdf@data$AU2013_NAM, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  mutate(AU2013 = as.integer(AU2013)) %>%
  select(long, lat, group, CAU = AU2013, CAU_NAME = AU2013_NAM)

devtools::use_data(chch_cau_13.df)


# Hamilton -------------------------------------------------

ham_meshblocks.df <- mb_info.df %>%
  filter(as.integer(TA2013) %in% c(13, 15:19))

# prepare the maps
ham_cau_13.df <- nz_map.spdf[(nz_map.spdf@data$AU2013 %in% ham_meshblocks.df$AU2013) & !(grepl("Estuary|Marinas|[iI]nlet|[Tt]idal|^[Hh]arbour|[Oo]ceanic|Tamaki Strait|^Bays(?!water)|Water", x = nz_map.spdf@data$AU2013_NAM, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  mutate(AU2013 = as.integer(AU2013)) %>%
  select(long, lat, group, CAU = AU2013, CAU_NAME = AU2013_NAM)

devtools::use_data(ham_cau_13.df)


# 2006 AREA UNITS ---------------------------------------------------------

gis_dir <- "M:/gisdata/census2006"

# directory for the map data
nz_map.spdf <- readOGR(dsn = gis_dir, layer = "au", stringsAsFactors = FALSE)
nz_map.spdf$id <- rownames(nz_map.spdf@data)

# meshblock data frame
mb_info.df <- readOGR(dsn = gis_dir, layer = "mb", stringsAsFactors = FALSE)@data

nz_cau_06.spdf <- nz_map.spdf
devtools::use_data(nz_cau_06.spdf)


# Auckland ----------------------------------------------------------------

akl_meshblocks <- mb_info.df %>%
  filter(as.integer(TA06) %in% c(4:10)) %>%
  .$AU06

# prepare the map
akl_cau_06.df <- nz_map.spdf[(nz_map.spdf@data$AU_NO %in% akl_meshblocks) & !(grepl("Marinas|[iI]nlet|[Tt]idal|^[Hh]arbour|[Oo]ceanic|Tamaki Strait|^Bays(?!water)", x = nz_map.spdf@data$AU_NAME, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  select(long, lat, group, CAU = AU_NO, CAU_NAME = AU_NAME)

devtools::use_data(akl_cau_06.df)


# Wellington --------------------------------------------------------------

wtn_meshblocks <- mb_info.df %>%
  filter(as.integer(TA06) %in% c(43:47, 50)) %>%
  .$AU06

# prepare the map
wtn_cau_06.df <- nz_map.spdf[(nz_map.spdf@data$AU_NO %in% wtn_meshblocks) & !(grepl("Marinas|[iI]nlet|[Tt]idal|^[Hh]arbour|[Oo]ceanic|Tamaki Strait|^Bays(?!water)", x = nz_map.spdf@data$AU_NAME, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  select(long, lat, group, CAU = AU_NO, CAU_NAME = AU_NAME)

devtools::use_data(wtn_cau_06.df)


# Christchurch ------------------------------------------------------------

chch_meshblocks <- mb_info.df %>%
  filter(as.integer(TA06) %in% c(60, 59, 61, 62)) %>%
  .$AU06

# prepare the map
chch_cau_06.df <- nz_map.spdf[(nz_map.spdf@data$AU_NO %in% chch_meshblocks) & !(grepl("Marinas|[iI]nlet|[Tt]idal|^[Hh]arbour|[Oo]ceanic|Tamaki Strait|^Bays(?!water)", x = nz_map.spdf@data$AU_NAME, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  select(long, lat, group, CAU = AU_NO, CAU_NAME = AU_NAME)

devtools::use_data(chch_cau_06.df)

# Hamilton ------------------------------------------------------------

ham_meshblocks <- mb_info.df %>%
  filter(as.integer(TA06) %in% c(13, 15:19)) %>%
  .$AU06

# prepare the map
ham_cau_06.df <- nz_map.spdf[(nz_map.spdf@data$AU_NO %in% ham_meshblocks) & !(grepl("Marinas|[iI]nlet|[Tt]idal|^[Hh]arbour|[Oo]ceanic|Tamaki Strait|^Bays(?!water)", x = nz_map.spdf@data$AU_NAME, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  select(long, lat, group, CAU = AU_NO, CAU_NAME = AU_NAME)

devtools::use_data(ham_cau_06.df)
