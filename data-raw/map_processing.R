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
gis_dir <- "M:/gisdata/2013 Boundaries/ESRI shapefile Output/2013 Digital Boundaries Generlised Clipped"

# Import the TLA shapefile
nz_tla_13.spdf <- readOGR(dsn = gis_dir, layer = "TA2013_GV_Clipped", stringsAsFactors = FALSE)
nz_tla_13.spdf$id <- rownames(nz_tla_13.spdf@data)

# Save the TLA data ----
nz_tla_13.spdf %<>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  SpatialPolygonsDataFrame(nz_tla_13.spdf@data)

# save the tla shapefile
devtools::use_data(nz_tla_13.spdf, overwrite = TRUE)

# Write a data.frame of the TLAs ----
nz_tla_13.df <- nz_tla_13.spdf %>%
  fortify(region = "id") %>%
  left_join(nz_tla_13.spdf@data, by = "id") %>%
  mutate(TA2013 = as.integer(TA2013)) %>%
  select(long, lat, group, TLA = TA2013, TLA_NAME = TA2013_NAM) %>%
  filter(TLA_NAME != "Chatham Islands Territory")

devtools::use_data(nz_tla_13.df, overwrite = TRUE)

# Import the Region shapefile ----
nz_regions.spdf <- readOGR(dsn = gis_dir, layer = "REGC2013_GV_Clipped", stringsAsFactors = FALSE)
nz_regions.spdf$id <- rownames(nz_regions.spdf@data)

# Save the TLA data ----
nz_regions.spdf %<>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  SpatialPolygonsDataFrame(nz_regions.spdf@data)

# save the tla shapefile
devtools::use_data(nz_regions.spdf, overwrite = TRUE)

# Write a data.frame of the Regions ----
nz_regions.df <- nz_regions.spdf %>%
  fortify(region = "id") %>%
  left_join(nz_regions.spdf@data, by = "id") %>%
  mutate(REGC2013 = as.integer(REGC2013)) %>%
  select(long, lat, group, REGION = REGC2013, REGION_NAME = REGC2013_N) %>%
  filter(REGION_NAME != "Area Outside Region")

devtools::use_data(nz_regions.df, overwrite = TRUE)


# import the full area unit shapefile
nz_map.spdf <- readOGR(dsn = gis_dir, layer = "AU2013_GV_Clipped", stringsAsFactors = FALSE)
nz_map.spdf$id <- rownames(nz_map.spdf@data)

# import the data from the meshblock shapefile
mb_map.spdf <- readOGR(dsn = gis_dir, layer = "MB2013_GV_Clipped", stringsAsFactors = FALSE)

mb_info.df <- mb_map.spdf@data

nz_cau_13.spdf <- nz_map.spdf %>%
  subset(!grepl("Inland Water", x = nz_map.spdf@data$AU2013_NAM))
names(nz_cau_13.spdf) <- c("CAU", "CAU_NAME", "id")
devtools::use_data(nz_cau_13.spdf, overwrite = TRUE)

# Auckland -----------------------------------------------------

akl_meshblocks.df <- mb_info.df %>%
  filter(as.integer(TA2013) %in% c(76))

# prepare the maps
akl_cau_13.df <- nz_map.spdf[(nz_map.spdf@data$AU2013 %in% akl_meshblocks.df$AU2013) & !(grepl("Inland Water", x = nz_map.spdf@data$AU2013_NAM, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  mutate(AU2013 = as.integer(AU2013)) %>%
  select(long, lat, group, CAU = AU2013, CAU_NAME = AU2013_NAM)

devtools::use_data(akl_cau_13.df, overwrite = TRUE)



# Wellington ---------------------------------------------------

wgtn_meshblocks.df <- mb_info.df %>%
  filter(as.integer(TA2013) %in% c(43:47, 50))

# prepare the maps
wtn_cau_13.df <- nz_map.spdf[(nz_map.spdf@data$AU2013 %in% wgtn_meshblocks.df$AU2013) & !(grepl("Inland Water", x = nz_map.spdf@data$AU2013_NAM, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  mutate(AU2013 = as.integer(AU2013)) %>%
  select(long, lat, group, CAU = AU2013, CAU_NAME = AU2013_NAM)

devtools::use_data(wtn_cau_13.df, overwrite = TRUE)

# Meshblocks ----
mb_map.spdf %<>%
  subset(as.integer(TA2013) %in% c(43:47, 50) & !(grepl("Inland Water", x = mb_map.spdf@data$AU2013_NAM)))
mb_map.spdf$id <- row.names(mb_map.spdf@data)

wtn_mb_13.df <- mb_map.spdf %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(mb_map.spdf@data, by = "id") %>%
  mutate(MB = as.integer(MB2013)) %>%
  select(long, lat, group, MB, CAU = AU2013)


devtools::use_data(wtn_mb_13.df, overwrite = TRUE)


# Christchurch -------------------------------------------------

chch_meshblocks.df <- mb_info.df %>%
  filter(as.integer(TA2013) %in% c(60, 59, 62))

# prepare the maps
chch_cau_13.df <- nz_map.spdf[(nz_map.spdf@data$AU2013 %in% chch_meshblocks.df$AU2013) & !(grepl("Inland Water", x = nz_map.spdf@data$AU2013_NAM, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  mutate(AU2013 = as.integer(AU2013)) %>%
  select(long, lat, group, CAU = AU2013, CAU_NAME = AU2013_NAM)

devtools::use_data(chch_cau_13.df, overwrite = TRUE)


# Hamilton -------------------------------------------------

ham_meshblocks.df <- mb_info.df %>%
  filter(as.integer(TA2013) %in% c(13, 15:19))

# prepare the maps
ham_cau_13.df <- nz_map.spdf[(nz_map.spdf@data$AU2013 %in% ham_meshblocks.df$AU2013) & !(grepl("Inland Water", x = nz_map.spdf@data$AU2013_NAM, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  mutate(AU2013 = as.integer(AU2013)) %>%
  select(long, lat, group, CAU = AU2013, CAU_NAME = AU2013_NAM)

devtools::use_data(ham_cau_13.df, overwrite = TRUE)

# Tauranga -------------------------------------------------

tga_meshblocks.df <- mb_info.df %>%
  filter(as.integer(TA2013) %in% c(22, 23))

# prepare the maps
tga_cau_13.df <- nz_map.spdf[(nz_map.spdf@data$AU2013 %in% tga_meshblocks.df$AU2013) & !(grepl("Inland Water", x = nz_map.spdf@data$AU2013_NAM, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  mutate(AU2013 = as.integer(AU2013)) %>%
  select(long, lat, group, CAU = AU2013, CAU_NAME = AU2013_NAM)

devtools::use_data(tga_cau_13.df, overwrite = TRUE)

# Meshblocks ----
mb_map.spdf %<>%
  subset(as.integer(TA2013) %in% c(22, 23) & !(grepl("Inland Water", x = mb_map.spdf@data$AU2013_NAM)))
mb_map.spdf$id <- row.names(mb_map.spdf@data)

tga_mb_13.df <- mb_map.spdf %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(mb_map.spdf@data, by = "id") %>%
  mutate(MB = as.integer(MB2013)) %>%
  select(long, lat, group, MB, CAU = AU2013)


devtools::use_data(tga_mb_13.df, overwrite = TRUE)


# 2006 AREA UNITS ---------------------------------------------------------

gis_dir <- "M:/gisdata/census2006"

# directory for the map data
nz_map.spdf <- readOGR(dsn = gis_dir, layer = "au", stringsAsFactors = FALSE)
nz_map.spdf$id <- rownames(nz_map.spdf@data)

# meshblock data frame
mb_info.df <- readOGR(dsn = gis_dir, layer = "mb", stringsAsFactors = FALSE)@data

nz_cau_06.spdf <- nz_map.spdf
names(nz_cau_06.spdf) <- c("CAU", "CAU_NAME", "id")
devtools::use_data(nz_cau_06.spdf, overwrite = TRUE)

# Write the 2006 TLA data ----
nz_tla_06.spdf <- readOGR(dsn = gis_dir, layer = "ta", stringsAsFactors = FALSE)
nz_tla_06.spdf$id <- rownames(nz_tla_06.spdf@data)

# Save the TLA data ----
nz_tla_06.spdf %<>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  SpatialPolygonsDataFrame(nz_tla_06.spdf@data)

# save the tla shapefile
devtools::use_data(nz_tla_06.spdf, overwrite = TRUE)

# Write a data.frame of the TLAs ----
nz_tla_06.df <- nz_tla_06.spdf %>%
  fortify(region = "id") %>%
  left_join(nz_tla_06.spdf@data, by = "id") %>%
  mutate(TA_NO = as.integer(TA_NO)) %>%
  select(long, lat, group, TLA = TA_NO, TLA_NAME = TA_NAME) %>%
  filter(!(TLA_NAME %in% c("Tasman District", "Marlborough District"))) %>%
  bind_rows(
    nz_tla_13.df %>%
      filter(TLA_NAME %in% c("Tasman District", "Marlborough District"))
  ) %>%
  mutate(group = ifelse(TLA == 13, 999, group))

devtools::use_data(nz_tla_06.df, overwrite = TRUE)



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


# Tauranga -------------------------------------------------

tga_meshblocks <- mb_info.df %>%
  filter(as.integer(TA06) %in% c(22, 23)) %>%
  .$AU06

# prepare the map
tga_cau_06.df <- nz_map.spdf[(nz_map.spdf@data$AU_NO %in% tga_meshblocks) & !(grepl("Marinas|[iI]nlet|[Tt]idal|^[Hh]arbour|[Oo]ceanic|Tamaki Strait|^Bays(?!water)", x = nz_map.spdf@data$AU_NAME, perl = TRUE)), ] %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(nz_map.spdf@data, by = "id") %>%
  select(long, lat, group, CAU = AU_NO, CAU_NAME = AU_NAME)

devtools::use_data(tga_cau_06.df)



# Meshblocks ----
mb_map.spdf <- readOGR(dsn = gis_dir, layer = "mb", stringsAsFactors = FALSE)
mb_map.spdf$id <- row.names(mb_map.spdf@data)
mb_map.spdf@data %<>% left_join(nz_map.spdf@data %>% select(AU_NO, AU_NAME), by = c("AU06" = "AU_NO"))

mb_map.spdf %<>%
  subset(as.integer(TA06) %in% c(22, 23) & !(grepl("Estuary|Marinas|[iI]nlet|[Tt]idal|^[Hh]arbour|[Oo]ceanic|Tamaki Strait|^Bays|Water", x = mb_map.spdf@data$AU_NAME)))

tga_mb_06.df <- mb_map.spdf %>%
  gSimplify(tol = 25, topologyPreserve = TRUE) %>%
  fortify(region = "id") %>%
  left_join(mb_map.spdf@data, by = "id") %>%
  mutate(MB = as.integer(MB06)) %>%
  select(long, lat, group, MB, CAU = AU06)


devtools::use_data(tga_mb_06.df)
