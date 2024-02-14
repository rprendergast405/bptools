# latlon_to_nztm
# ------------------------------------------------------------------------
# A function to transform spatial coordinates from lat-long projection to
# NZTM
# -------------------------------------------------------------------------
# Created by Bert, 06-10-2016
# -------------------------------------------------------------------------

#' Convert lat-long Coordinates to NZTM Projection
#'
#' @param dat
#'
#' @export latlon_to_nztm
latlon_to_nztm <- function(dat) {

  transformed_dat <- dplyr::select(dat, longitude, latitude)
  transformed_dat <- sp::SpatialPoints(transformed_dat, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  transformed_dat <- sp::spTransform(transformed_dat, "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs")
  transformed_dat <- as.data.frame(transformed_dat)
  transformed_dat <- dplyr::rename(transformed_dat, longitude_nztm = longitude, latitude_nztm = latitude)

  return(cbind(dat, transformed_dat))
}


#' Convert NZTM Coordinates to lat-long Projection
#'
#' @param dat
#' @export nztm_to_latlon
nztm_to_latlon <- function(dat) {

  transformed_dat <- dplyr::select(dat, longitude, latitude)
  transformed_dat <- sp::SpatialPoints(transformed_dat, proj4string = sp::CRS("+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"))
  transformed_dat <- sp::spTransform(transformed_dat, "+proj=longlat +datum=WGS84")
  transformed_dat <- as.data.frame(transformed_dat)
  transformed_dat <- dplyr::rename(transformed_dat, longitude_nztm = longitude, latitude_nztm = latitude)

  return(cbind(dat, transformed_dat))
}
