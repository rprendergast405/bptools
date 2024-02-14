#' Fetch the location of a merchant from Fozzie.
#'
#' Given a group ID, \code{get_merchant_location()} returns a data frame containing the coordinates of the merchant, if it is available on Fozzie.
#'
#' @param db_con A connection to the BNZ database
#' @param group_id The group ID of the merchant of interest
#'
#' @export get_merchant_location
#'
#' @examples
#' DB <- odbcConnect("MVIEW", uid="user", pwd="password")
#' get_merchant_location(DB, group_id = 15435)
get_merchant_location <- function(db_con, group_id) {

  # I suppose I'd better try and prevent sql injections
  if(!is.numeric(group_id)) stop("group_id must be numeric")
  if(length(group_id) != 1) stop("you should only provide one group_id")

  db_qry <- paste0("
SELECT        X_COORD, X_NZTM, Y_COORD, Y_NZTM
FROM          FOZZIE.GROUPED_RECEIVER gr
INNER JOIN    FOZZIE.GROUPED_RECEIVER_LOCATION grl ON gr.OID = grl.GROUP_ID
LEFT JOIN     census.mb06_centroids cent on grl.MESHBLOCK = cent.MB06_NUM

WHERE         OID = ", group_id)

  dat <- RODBC::sqlQuery(db_con, db_qry, stringsAsFactors = FALSE)

  if(is.na(dat$X_COORD)) {
    warning(paste0("Exact coordinates not available for merchant ", group_id, "; using imputed location"))

    dat$X_COORD <- dat$X_NZTM
    dat$Y_COORD <- dat$Y_NZTM
  }

  dat <- dat[, names(dat) %in% c("X_COORD", "Y_COORD")]

  return(dat)
}


#' Get the origin of BNZ spending for a given merchant.
#'
#' Given a group ID and a time frame, \code{bnz_spend_origin()} returns a data frame containing the spending by customer CAU.
#'
#' There might be some issues with the default date ranges if the current date is at the start or end of the month, but it
#' should be good enough and you can set it manually if you're concerned.
#'
#' @param db_con A connection to the BNZ database
#' @param group_id The group ID(s) of the merchant(s) of interest
#' @param seqmonth_start The first month from which to get spending. Defaults to 13 months prior to Sys.Date()
#' @param seqmonth_end The last month from which to get spending. Defaults to one month prior to Sys.Date()
#'
#' @export bnz_spend_origin
#'
#' @examples
#' DB <- odbcConnect("MVIEW", uid="user", pwd="password")
#' bnz_spend_origin(DB, group_id = 15435)
bnz_spend_origin <- function(db_con, group_id,
                             seqmonth_start = as.numeric(format(min(seq.Date(from = Sys.Date(), by = "-1 months", length.out = 13)), "%Y%m")),
                             seqmonth_end = as.numeric(format(seq.Date(from = Sys.Date(), by = "-1 months", length.out = 13)[2], "%Y%m"))) {

  # I suppose I'd better try and prevent sql injections
  if (!is.numeric(group_id)) stop("group_id must be numeric")
  if (!is.numeric(seqmonth_start)) stop("seqmonth_start must be numeric")
  if (length(seqmonth_start) != 1) stop("you should only provide one seqmonth_start")
  if (!is.numeric(seqmonth_end)) stop("seqmonth_end must be numeric")
  if (length(seqmonth_end) != 1) stop("you should only provide one seqmonth_end")


  db_qry <- paste0("
SELECT      el.group_id AS merch_id, mb06.cau_id AS cau06, mb13.cau_id AS cau13,
SUM(bnz.TRANSACTION_VALUE) AS SPEND

FROM        BNZTRANS.BNZTRANS bnz

INNER JOIN  fozzie.grouped_elements el
            ON bnz.receiver_id = el.element_id
            AND bnz.receiver_type = el.element_type

INNER JOIN  bnztrans.customer_ext_meshblocks c
            ON bnz.customer_id = c.customer_id
LEFT JOIN   census.meshblock_2006 mb06
            ON mb06.id = c.meshblock_06
LEFT JOIN   census.meshblock_2013 mb13
            ON mb13.id = c.meshblock_13

WHERE       bnz.SEQMONTH BETWEEN ", seqmonth_start, " AND ", seqmonth_end, "
            AND el.group_id in (", paste(group_id, collapse = ", "), ")

GROUP BY    el.group_id, mb06.cau_id, mb13.cau_id"
  )

  dat <- RODBC::sqlQuery(db_con, db_qry, stringsAsFactors = FALSE)

  dat <- dplyr::as.tbl(dat)

  return(dat)
}



#' Import a meshblock shapefile
#'
#' A function to import a shapefile of meshblock boundaries for a given census definition.
#'
#' @param census Which census meshblocks should be retrieved?
#'
#' @return SpatialPolygonsDataFrame describing the meshblock boundaries
#' @export get_meshblock_data
#'
#' @examples mb_map.df <- get_meshblock_data(2013)
get_meshblock_data <- function(census = 2013) {
  if (census == 2013) {
    dat <- sf::st_read(dsn = "M:/gisdata/2013 Boundaries/ESRI shapefile Output/2013 Digital Boundaries Generlised Clipped",
                       layer = "MB2013_GV_Clipped", stringsAsFactors = FALSE)

    # Remove z attributes
    dat <- sf::st_zm(dat)

    # Convert to spatial
    dat <- as(dat, "Spatial")

    # Add a MB column describing the meshblock
    dat$MB <- as.integer(dat$MB2013)

    # Add an ID column for fortifying
    dat$id <- rownames(dat@data)

  } else if (census == 2006) {
    dat <- sf::st_read(dsn = "M:/gisdata/census2006",
                       layer = "mb", stringsAsFactors = FALSE)

    # Remove z attributes
    dat <- sf::st_zm(dat)

    # Convert to spatial
    dat <- as(dat, "Spatial")

    # Add a MB column describing the meshblock
    dat$MB <- as.integer(dat$MB06)

    # Add an ID column for fortifying
    dat$id <- rownames(dat@data)

  } else stop("census should be either 2013 or 2006.")
return(dat)
}



#' Create a data.frame of meshblock boundaries
#'
#' Given a meshblock shapefile and a list of meshblocks, make_mb_df selects the meshblocks and fortifies them into a data.frame for plotting
#'
#' @param mb_map.spdf shapefile of the meshblock boundaries
#' @param mbs subset of the meshblocks to get
#'
#' @export make_mb_df
#'
#' @import sp
make_mb_df <- function(mb_map, mbs, tol = 25, agg = FALSE, agg_name = "aggregated"){
  # simplify and fortify the data
  mb_df <- mb_map %>%
    subset(MB %in% mbs)

  if (agg) {
    mb_df$agg <- agg_name

    mb_df <- mb_df %>%
      sf::st_as_sf() %>%
      aggregate(., list(.$agg), "first") %>%
      sf::st_as_sf(sf_column_name = geometry) %>%
      as("Spatial")
    }

  mb_df <- mb_df %>%
    rgeos::gSimplify(tol = tol, topologyPreserve = TRUE) %>%
    ggplot2::fortify(region = "id") %>%
    dplyr::left_join(mb_map@data, by = "id") %>%
    dplyr::select(long, lat, group, MB) %>%
    dplyr::distinct()

  return(mb_df)
}

#' Create a data.frame of area unit boundaries
#'
#' Given a CAU shapefile and a list of area units, make_cau_df selects the area units and fortifies them into a data.frame for plotting
#'
#' @param mb_map.spdf shapefile of the area unit boundaries
#' @param mbs subset of the area units to get
#'
#' @export make_cau_df
#'
#' @import sp
make_cau_df <- function(cau_map, caus, tol = 25, agg = FALSE, agg_name = "aggregated") {

  cau_df <- cau_map %>%
    subset(CAU %in% caus)

  if (agg) {
    cau_df$agg <- agg_name

    cau_df <- cau_df %>%
      sf::st_as_sf() %>%
      aggregate(., list(.$agg), "first") %>%
      sf::st_as_sf(sf_column_name = geometry) %>%
      as("Spatial")
  }

  cau_df <- cau_df %>%
    rgeos::gSimplify(tol = tol, topologyPreserve = TRUE) %>%
    ggplot2::fortify(region = "id") %>%
    dplyr::left_join(cau_map@data, by = "id") %>%
    dplyr::select(long, lat, group, hole, CAU, CAU_NAME) %>%
    dplyr::mutate(CAU = as.integer(CAU)) %>%
    dplyr::distinct()

  return(cau_df)
}


#' Parse a String Vector for SQL Queries
#'
#' @param vec A vector of strings to convert to a SQL list
#'
#' @return A string containing the values in \code{vec} in SQL-list format
#' @export vec_sql_string

vec_sql_string <- function(vec){

  #Only accept classes which can be interpretted as strings
  stopifnot(is.numeric(vec) | is.character(vec) | is.factor(vec) | is.integer(vec))

  if(any(grepl(";|'(!?=')", vec, perl = TRUE))) stop("You need to sanitise your input vector - semicolons and unescaped quotations are not allowed")

  if(is.numeric(vec)){
    sql_vec <- paste0("(", paste(vec, collapse = ", "), ")")
  }

  if(is.character(vec) | is.factor(vec)){
    sql_vec <- paste0("(", paste(paste0("'", as.character(vec), "'"), collapse = ", "), ")")
  }

  return(sql_vec)

}


#A function to find all the merchants within a radius (in m) of a given location (in NZTM)
#Optionally include merchants with imputed location from MB centroid

#' Find Merchants Within a Given Radius
#'
#' A function to find all the merchants within a radius (in m) of a given location (in NZTM).
#' Optionally include merchants with imputed location from MB centroid.
#'
#' @param DB An ODBC connection to bespoke
#' @param x X coordinate of the centroid
#' @param y Y coordinate of the centroid
#' @param rad The radius in which to find merchants
#' @param impute Should merchants with imputed location be included in the results?
#'
#' @return A \code{data.frame} of the Fozzie IDs and locations of all merchants within the given radius
#' @export merch_radius_locator
merch_radius_locator <- function(DB, x, y, rad, impute = T){

  if(!is.numeric(c(x, y, rad))) stop("Your inputs need to be numeric")
  if(length(x) != 1 | length(y) != 1) stop("You must only supply a single location")
  if(length(rad) != 1) stop("You can only search within a single radius")


  #If exact do not use imputed location
  if(!impute){
    merchants <- RODBC::sqlQuery(DB, paste("select group_id, x_coord as x, y_coord as y
                     from fozzie.grouped_receiver_location
                     where round(sqrt(power((X_coord -",
                                    x,
                                    "),2)+power((Y_COORD - ",
                                    y,
                                    "),2))) < ",
                                    rad))
  } else{
    merchants <- RODBC::sqlQuery(DB, paste("select a.group_id, coalesce(x_coord, x_nztm) as x, coalesce(y_coord, y_nztm) as y
                                    from fozzie.grouped_receiver_location a
                                    left join CENSUS.MB06_CENTROIDS b on a.MESHBLOCK = b.MB06_NUM
                                    where round(sqrt(power((coalesce(x_coord, x_nztm) -",
                                    x,
                                    "),2)+power((coalesce(y_coord, y_nztm) - ",
                                    y,
                                    "),2))) < ",
                                    rad
    ))
  }
  merchants <- dplyr::as.tbl(merchants)

  return(merchants)

}

#' Find IEO Merchants Within a Given Radius
#'
#' A function to find all the IEO merchants within a radius (in m) of a given location (in NZTM).
#' Optionally include merchants with imputed location from MB centroid.
#'
#' @param DB An ODBC connection to bespoke
#' @param x X coordinate of the centroid
#' @param y Y coordinate of the centroid
#' @param rad The radius in which to find merchants
#' @param impute Should merchants with imputed location be included in the results?
#'
#' @return A \code{data.frame} of the Fozzie IDs and locations of all merchants within the given radius
#' @export ieo_radius_locator
ieo_radius_locator <- function(DB, x, y, rad, impute = T){

  if(!is.numeric(c(x, y, rad))) stop("Your inputs need to be numeric")
  if(length(x) != 1 | length(y) != 1) stop("You must only supply a single location")
  if(length(rad) != 1) stop("You can only search within a single radius")


  #If exact do not use imputed location
  if(!impute){
    merchants <- RODBC::sqlQuery(DB, paste("select group_id, x_coord as x, y_coord as y
                                           from fozzie.grouped_receiver_location grl
                                           inner join mcd_bp_grp_ids ieo ON ieo.merch_id = grl.group_id
                                           where round(sqrt(power((X_coord -",
                                           x,
                                           "),2)+power((Y_COORD - ",
                                           y,
                                           "),2))) < ",
                                           rad))
  } else{
    merchants <- RODBC::sqlQuery(DB, paste("select a.group_id, coalesce(x_coord, x_nztm) as x, coalesce(y_coord, y_nztm) as y
                                           from fozzie.grouped_receiver_location a
                                           inner join mcd_bp_grp_ids ieo ON ieo.merch_id = a.group_id
                                           left join CENSUS.MB06_CENTROIDS b on a.MESHBLOCK = b.MB06_NUM
                                           where round(sqrt(power((coalesce(x_coord, x_nztm) -",
                                           x,
                                           "),2)+power((coalesce(y_coord, y_nztm) - ",
                                           y,
                                           "),2))) < ",
                                           rad
                                           ))
  }
  merchants <- dplyr::as.tbl(merchants)

  return(merchants)

}

#' Find Meshblocks Within a Given Radius
#'
#' A function to find all the meshblocks within a radius (in m) of a given location (in NZTM).
#'
#' @param DB An ODBC connection to bespoke
#' @param x X coordinate of the centroid
#' @param y Y coordinate of the centroid
#' @param rad The radius in which to find meshblocks
#'
#' @export mb_radius_locator
mb_radius_locator <- function(DB, x, y, rad, census = 2006){

  if (!is.numeric(c(x, y, rad))) stop("Your inputs need to be numeric")
  if (length(x) != 1 | length(y) != 1) stop("You must only supply a single location")
  if (length(rad) != 1) stop("You can only search within a single radius")

  census <- match.arg(as.character(census), c("2006", "2013"))

  if (census == "2006") {
    mb_data <- RODBC::sqlQuery(DB, gsub("\n", "", paste0("select distinct mb06, mb06_num
                                                from CENSUS.MB06_CENTROIDS
                                                where round(sqrt(power((x_nztm - ",
                                                         x,
                                                         "),2)+power((y_nztm - ",
                                                         y,
                                                         "),2))) <  ",
                                                         rad), fixed = T))
  } else if (census == "2013") {
    mb_data <- RODBC::sqlQuery(DB, gsub("\n", "", paste0("select distinct meshblock AS mb13
                                                from CENSUS.MB_CENTROIDS_2013
                                                         where round(sqrt(power((x_nztm - ",
                                                         x,
                                                         "),2)+power((y_nztm - ",
                                                         y,
                                                         "),2))) <  ",
                                                         rad), fixed = T))
  }


  mb_data <- dplyr::as.tbl(mb_data)

  return(mb_data)

}
