# mvl_functions.R
# -------------------------------------------------------------------------
# A script containing some misc day-to-day functions used in Marketview
# reporting
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

  if (length(proc_script) > 0) processing_time <- file.info(proc_script)$mtime

  # When was the data last updated?
  data_file <- file.path(data_dir, grep("*.\\.RData$", dir(data_dir), value = TRUE, ignore.case = TRUE))
  if (length(data_file) == 0) return("No data found. Check that your processed data has the .RData file type.")
  if (length(proc_script) > 0) data_time <- file.info(data_file)$mtime

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

  # When was the script updated?
  cat(paste("Data processing script last updated", crayon::red(format(processing_time, "%H:%M:%S, %a %b %d")), "\n"))

  # Load the processed files
  data_files <- split(data_file, 1:length(data_file))
  lapply(data_files, load_verbose)

  # Warn if the script has been updated, but the data hasn't
  data_time <- min(file.info(data_file)$mtime)
  if (processing_time > data_time) {
    warning("Your processing script has been updated more recently than your data.")
  }

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
