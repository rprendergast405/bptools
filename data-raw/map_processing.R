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

