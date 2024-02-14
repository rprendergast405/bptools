# bp_functions.R
# -------------------------------------------------------------------------
# A script containing some misc day-to-day functions used in reporting
# -------------------------------------------------------------------------
# Created by Bert on 05-07-2016
# -------------------------------------------------------------------------
# Last edited by Bert on 12-07-2016
# -------------------------------------------------------------------------


#' Remove whitespace from character vectors
#'
#' @param x A character vector
#'
#' @return Text vector with leading and trailing whitespace removed
#' @export trim_ws
#'
#' @examples
#' trim_ws(c("test_name", "text_artifact       "))
trim_ws <- function(x){
  gsub("^\\s+|\\s+$", "", x)
}


#' Archive previous results from a project
#'
#' @param base_dir The base directory of the project
#'
#' @export output_archive

output_archive <- function(base_dir = getwd()) {
  # The directory containing the outputs
  output_dir <- file.path(base_dir, "output")

  # Archive previous versions of the outputs
  archive_time <-  gsub(x = gsub(x = Sys.time(), pattern = " ", replacement = "_"), pattern = "[^0-9a-z_]", replacement = "")

  # create an archive for old files
  dir.create(path = file.path(base_dir, "archive", archive_time), recursive = TRUE)

  # copy the old files
  file.copy(from = output_dir, to = file.path(base_dir, "archive", archive_time), recursive = TRUE)

  # remove the files once they are archived
  file.remove(list.files(output_dir, full.names = TRUE, recursive = TRUE))
}



#' Import the processed data for a project
#'
#' @param data_dir The directory where the processed data lives
#' @param r_dir The directory where the processing scripts live
#'
#' @return imports the processed data into the global environment
#' @export data_import
data_import <- function(data_dir = file.path("data", "processed"), r_dir = file.path("R")) {

  # When was the processing script last updated?
  proc_script <- file.path("R", grep("*.processing\\.R$", dir(r_dir), value = TRUE))

  # When was the data last updated?
  data_file <- file.path(data_dir, grep("*.\\.RData$", dir(data_dir), value = TRUE, ignore.case = TRUE))
  if (length(data_file) == 0) return("No data found. Check that your processed data has the .RData file type.")
  if (length(proc_script) > 0) data_time <- file.info(data_file)$mtime



  if (length(proc_script) > 0) processing_time <- file.info(proc_script)$mtime else processing_time <- NA

  if (is.na(processing_time)) {
    warning("There's no processing script.")
  } else {

    # When was the script updated?
    cat(paste("Data processing script last updated", crayon::red(format(processing_time, "%H:%M:%S, %a %b %d")), "\n"))

    # Warn if the script has been updated, but the data hasn't
    data_time <- min(file.info(data_file)$mtime)
    if (processing_time > data_time) {
      warning("Your processing script has been updated more recently than your data.")
    }

  }

  # Warn if there are multiple files in the directory
  if (length(data_file) > 1) {
    cat(paste0(length(data_file), " .RData files found in ", crayon::green(data_dir), ". Use load() if you only need to import particular files.\n"))
  }


  # Import the data
  load_verbose <- function(path) {
    # Last updated?
    data_time <- file.info(path)$mtime

    short_path <- gsub(paste0(data_dir, "/"), "", path)

    # tell the user what's happening
    cat(paste0("Importing ", crayon::green(short_path), " (last updated ", crayon::red(format(data_time, "%H:%M:%S, %a %b %d")), ")\n"))

    # Import the data to the global env
    load(path, envir = .GlobalEnv)
  }


  # Load the processed files
  data_files <- split(data_file, 1:length(data_file))
  lapply(data_files, load_verbose)



}



#' Source all scripts in a directory
#'
#' @param path The directory containing the scripts
#' @param trace Print status to the console?
#' @param ... Other options
#'
#' @export source_dir
source_dir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if (trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if (trace) cat("\n")
  }
}



#' Compound Annual Growth Rate
#'
#' @param iv Initial value
#' @param fv Final value
#' @param length Length of time between \code{iv} and \code{fv}
#'
#' @return cagr The compound annual growth rate from \code{iv} to \code{fv}
#' @export cagr
#'
#' @examples
#' sales <- data.frame(year_1 = c(10, 5, 1), year_2 = c(15, 4, 5), year_3 = c(20, 8, 2))
#' cagr(iv = sales$year_1, fv = sales$year_3, length = 2)
cagr <- function(iv, fv, length){
  stopifnot(is.numeric(iv) & is.numeric(fv) & is.numeric(length))

  return((fv / iv) ^ (1 / length) - 1)
}


#' Parse SEQMONTH attributes as dates
#'
#' @param x A vector of SEQMONTH
#'
#' @export parse_seqmonth
parse_seqmonth <- function(x) {
  if (unique(nchar(x)) != 6) {
    stop("You should only provide SEQMONTH attributes")
  }

  x_date <- as.Date(paste0(x, "01"), format = "%Y%m%d")
}

#' Parse Date attributes to SEQDAY format
#'
#' @param x A vector of dates
#'
#' @export to_seqday
to_seqday <- function(x) {
  return(as.integer(format(x, "%Y%m%d")))
}


#' Add Spending across Given Attributes
#'
#' A convenience function for quickly adding the spending across attributes.
#'
#' @param dat A data frame to add the spending
#' @param ... Attributes to group by
#'
#' @export sum_spend
sum_spend <- function(dat, ..., var = SPEND, group = FALSE) {
  atts <- dplyr::quos(...)
  var <- dplyr::enquo(var)
  var_name <- dplyr::quo_name(var)

  dat <- dplyr::group_by(dat, !!!atts)
  dat <- dplyr::summarise(dat, !!var_name := sum(!!var, na.rm = TRUE))

  if (group == FALSE) {dat <- dplyr::ungroup(dat)}

  return(dat)
}


#' Initialise the Workspace for Analysis
#'
#' Essentially a function version of the standard MVL project initialise script. Will likely replace this script in the future.
#'
#' @export mvl_init
mvl_init <- function() {
  library(RODBC)
  library(tools)
  library(magrittr)
  library(tidyverse)
  library(stringr)
  library(forcats)
  library(scales)
  library(lubridate)
  library(readxl)
  library(bptools)
  library(mvldata)
  library(glue)
  library(ReporteRs)
  library(RcppRoll)


  # Set the plot theme -----
  theme_set(theme_mvl())

  # set colour palettes -----
  colour_discrete <- partial(scale_colour_mvl, palette = "mvl")
  colour_continuous <- partial(scale_colour_mvlc, palette = "Sky")
  fill_discrete <- partial(scale_fill_mvl, palette = "mvl")
  fill_continuous <- partial(scale_fill_mvlc, palette = "Sky")

  assign("scale_colour_discrete", colour_discrete, envir = .GlobalEnv)
  assign("scale_colour_continuous", colour_continuous, envir = .GlobalEnv)
  assign("scale_fill_discrete", fill_discrete, envir = .GlobalEnv)
  assign("scale_fill_continuous", fill_continuous, envir = .GlobalEnv)


  # make geom_col() reverse stack by default ----
  gcol_fn <- function(mapping = NULL,
                      data = NULL,
                      position = position_stack(reverse = TRUE),
                      ...,
                      width = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
    ggplot2::geom_col(
      mapping = mapping,
      data = data,
      position = position,
      ... = ...,
      width = width,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes
    )
  }

  assign("geom_col", gcol_fn, envir = .GlobalEnv)


  grDevices::windowsFonts(
    calb = "Calibri Bold",
    cal = "Calibri",
    hn = "Helvetica Neue",
    hnb = "Helvetica Neue Bold",
    cen = "Century Gothic",
    geo = "Georgia Italic"
  )

  # Set ReporteRs defaults
  options(
    "ReporteRs-default-font" = "Helvetica Neue",
    "ReporteRs-fontsize" = 12,
    "ReporteRs-locale.region" = "GB"
  )

  # Prevent print.data.frame from destroying your session ----
  print_fn <- function(x, ..., n = NULL, width = NULL) {
    x <- dplyr::as.tbl(x)
    print(x, ..., n = NULL, width = NULL)
  }

  assign("print.data.frame", print_fn, envir = .GlobalEnv)

  # Set stringsAsFactors to FALSE
  options("stringsAsFactors" = FALSE,
          "max.print" = 200)


}


#' Spread a key-value pair across multiple columns.
#'
#' This is a wrapper for \code{tidyr::spread}, which has an additional argument \code{...}, where you can list any other attributes other than \code{key} and \code{value} that should be kept in the output. All other attributes are dropped.
#'
#' @param data A data frame.
#' @param key Column names or positions. This is passed to tidyselect::vars_pull().
#' @param value These arguments are passed by expression and support quasiquotation (you can unquote column names or column positions).
#' @param ... Which additional columns in \code{data} should be kept in the output?
#' @param fill If set, missing values will be replaced with this value. Note that there are two types of missingness in the input: explicit missing values (i.e. NA), and implicit missings, rows that simply aren't present. Both types of missing value will be replaced by fill.
#' @param convert If TRUE, type.convert() with asis = TRUE will be run on each of the new columns. This is useful if the value column was a mix of variables that was coerced to a string. If the class of the value column was factor or date, note that will not be true of the new columns that are produced, which are coerced to character before type conversion.
#' @param drop If FALSE, will keep factor levels that don't appear in the data, filling in missing combinations with fill.
#' @param sep If NULL, the column names will be taken from the values of key variable. If non-NULL, the column names will be given by "<key_name><sep><key_value>".
#'
#' @export spread2
spread2 <- function(data, key, value, ..., fill = NA, convert = FALSE, drop = TRUE,
                    sep = NULL) {
  keyq <- enquo(key)
  valueq <- enquo(value)
  keep <- enquos(...)

  data <- dplyr::select(data, !!keyq, !!valueq, !!!keep)

  # data$rownum <- 1:nrow(data)

  out <- tidyr::spread(data = data, key = !!keyq, value = !!valueq, fill = fill, convert = convert, drop = drop, sep = sep)

  # out <- dplyr::select(out, -rownum)

  return(out)
}


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


#' Create a new R script
#' Creates a new R script with the headers and workflow mapped out in the same
#' manner as a standard results scrpt from create_project()
#'
#'
#' @param name The name for the new R script
#' @param location Directory where the script should be created - default is the /R directory
#'
#' @export create_script
create_script <- function(name, location = "R") {

  project_dir <- getwd()
  project_name <- gsub("^.*/", "", project_dir)

  script_text <- c("# ", name, ".R
# Created by ", Sys.info()["user"], ", on ", format(Sys.Date(), "%d %m %Y"), "
# -------------------------------------------------------------------------
# A script designed to ", name, " for the ", project_name, " project
# -------------------------------------------------------------------------
# WORKFLOW
#   0. INITIALISE
#   1. IMPORT DATA
#   2. RESULTS HERE
# -------------------------------------------------------------------------
# Last edited ", format(Sys.Date(), "%d %m %Y"), " by create_project()
#   - Created
# -------------------------------------------------------------------------

# 0. INITIALISE -----------------------------------------------------------

source(file.path(\"R\", \"0 ", project_name, " initialise.R\"))

# 1. IMPORT DATA ----------------------------------------------------------

data_import()
")

  cat(paste(script_text, collapse = ""), file = file.path(location, paste0(name, ".R")))
}


#' Create a new R Markdown script
#' Creates a new R Markdown script with the headers and workflow mapped out in the same
#' manner as a standard scrpt from create_project()
#'
#'
#' @param name The name for the new RMd script
#' @param location Directory where the script should be created - default is the root directory
#'
#' @export create_rmd
create_rmd <- function(name, location = "/") {

  project_dir <- getwd()
  project_name <- gsub("^.*/", "", project_dir)

  # Get the templates if they don't exist
  if (!any(grepl("templates", dir(project_dir)))) {
    dir.create(file.path(project_dir, "templates"))
  }

  if (!any(grepl("Analytics Banner.jpg", dir(file.path(project_dir, "templates"))))) {
    file.copy(from = system.file("extdata", "Analytics Banner.jpg", package = "bptools"), #file.path("M:/R/mvl_template_old.pptx"),
              to = file.path(project_dir, "templates"))
  }

  if (!any(grepl(".css$", dir(file.path(project_dir, "templates"))))) {
    file.copy(from = system.file("extdata", "mvlstyle.css", package = "bptools"), #file.path("M:/R/mvl_template_old.pptx"),
              to = file.path(project_dir, "templates"))
  }








  script_text <- c("---
title: \"\"
output:
  html_document:
    css: templates/mvlstyle.css
    toc: TRUE
    toc_float: TRUE
    number_sections: TRUE
  editor_options:
    chunk_output_type: console
---

![](templates/Analytics Banner.jpg)

# **", name, "** {-}
*`r  gsub(\"^0\", \"\", format(Sys.Date(), \"%d %B, %Y\"))`*

```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.width = 8, fig.height = 5, fig.path = 'output/figures/rmd/',
echo = FALSE, warning = FALSE, message = FALSE)
```

```{r init, results = \"hide\"}

# 0. INITIALISE -----------------------------------------------------------

source(\"R/0 ", project_name, " initialise.R\")
library(knitr)

# 1. IMPORT DATA ----------------------------------------------------------


data_import()

```

")

  cat(paste(script_text, collapse = ""), file = file.path(project_dir, location, paste0(name, ".RMd")))
                   }


#' Vectorised if returning an ordered result
#'
#' This function allows you to vectorise multiple if and else if statements. It is an R equivalent of the SQL CASE WHEN statement.
#' An update from the dplyr implementation as it returns the result as an ordered factor in the order given by the formulae
#'
#' @inheritParams dplyr::case_when
#'
#' @export case_fwhen

case_fwhen <- function(...) {
  formulas <- rlang::dots_list(...)

  formula_chr <- lapply(formulas, function(x) as.character(x)[3])

  formula_chr <- do.call(rbind, formula_chr)

  formula_chr <- as.character(formula_chr)

  result <- dplyr::case_when(...)

  result <- factor(result, levels = formula_chr)

  return(result)
}
