# mvl_functions.R
# -------------------------------------------------------------------------
# A script containing some misc day-to-day functions used in Marketview
# reporting
# -------------------------------------------------------------------------
# Created by Bert on 05-07-2016
# -------------------------------------------------------------------------
# Last edited by Bert on 12-07-2016
# -------------------------------------------------------------------------




#' Label in dollars
#'
#' A modification of the \code{\link[scales]{dollar}} function from \code{scales}, which
#' is more appropriate for negative values
#' @param x A numeric dollar amount
#'
#' @return A character representation of x in dollars
#'
#' @examples
#' dollar(1000000)
#' dollar(c(-2000000, 15000000))
#' @export dollar
dollar <- function(x, dp = 0, form = "f", ...){
  paste0(ifelse(x < 0, "-", ""), "$", formatC(abs(x), digits = dp, format = form, big.mark = ",", ...))
}

#' Label with '+' for positive growth
#'
#' A modification of the \code{\link[scales]{comma}} function from \code{scales}, which
#' is more appropriate for movements
#' @param x A numeric amount
#'
#' @return A character representation of x in dollars
#'
#' @examples
#' comma_change(1000000)
#' comma_change(c(-2000000, 115000000))
#' @export comma_change
comma_change <- function(x, dp = 0, form = "f", ...){
  paste0(ifelse(x < 0, "-", ""), ifelse(x > 0, "+", ""), formatC(abs(x), digits = dp, format = form, big.mark = ",", ...))
}


#' Label in dollars, with '+' for positive growth
#'
#' A modification of the \code{\link[scales]{dollar}} function from \code{scales}, which
#' is more appropriate for negative values
#' @param x A numeric dollar amount
#'
#' @return A character representation of x in dollars
#'
#' @examples
#' dollar(1000000)
#' dollar(c(-2000000, 15000000))
#' @export dollar_change
dollar_change <- function(x, dp = 0, form = "f", ...){
  paste0(ifelse(x < 0, "-", ""), ifelse(x > 0, "+", ""), "$", formatC(abs(x), digits = dp, format = form, big.mark = ",", ...))
}

#' Label in millions of dollars
#'
#' A modification of the \code{\link[scales]{dollar}} function from \code{scales}, which
#' is more appropriate for plots of large dollar values
#' @param x A numeric dollar amount
#'
#' @return A character representation of x in millions of dollars
#'
#' @examples
#' mdollar(1000000)
#' mdollar(c(2000000, 15000000))
#' @export mdollar
mdollar <- function(x, dp = 2, form = "f", ...){
  paste0(ifelse(x < 0, "-", ""), "$", formatC(abs(x)/1e6, digits = dp, format = form, big.mark = ",", ...), "M")
}

#' Label in millions
#'
#' A modification of the \code{\link[scales]{comma}} function from \code{scales}, which
#' is more appropriate for plots of large values
#' @param x A numeric dollar amount
#'
#' @return A character representation of x in millions
#'
#' @examples
#' mcomma(1000000)
#' mcomma(c(2000000, 15000000))
#' @export mcomma
mcomma <- function(x, dp = 1, form = "f", ...){
  paste0(ifelse(x < 0, "-", ""), formatC(abs(x)/1e6, digits = dp, format = form, big.mark = ",", ...), "M")
}

#' Label in billions of dollars
#'
#' A modification of the \code{\link[scales]{dollar}} function from \code{scales}, which
#' is more appropriate for plots of large dollar values
#' @param x A numeric dollar amount
#'
#' @return A character representation of x in billions of dollars
#'
#' @examples
#' bdollar(1000000000)
#' bdollar(c(2000000000, 15000000000))
#' @export bdollar
bdollar <- function(x, dp = 2, form = "f", ...){
  paste0(ifelse(x < 0, "-", ""), "$", formatC(abs(x)/1e9, digits = dp, format = form, big.mark = ",", ...), "B")
}

#' Label in billions
#'
#' A modification of the \code{\link[scales]{comma}} function from \code{scales}, which
#' is more appropriate for plots of large values
#' @param x A numeric amount
#'
#' @return A character representation of x in billions
#'
#' @examples
#' bcomma(1000000000)
#' bcomma(c(2000000000, 15000000000))
#' @export bcomma
bcomma <- function(x, dp = 1, form = "f", ...){
  paste0(ifelse(x < 0, "-", ""), formatC(abs(x)/1e9, digits = dp, format = form, big.mark = ",", ...), "B")
}

#' Label in thousands of dollars
#'
#' A modification of the \code{\link[scales]{dollar}} function from \code{scales}, which
#' is more appropriate for plots of large dollar values
#' @param x A numeric dollar amount
#'
#' @return A character representation of x in thousands of dollars
#'
#' @examples
#' kdollar(10000)
#' kdollar(c(2000, 1500000))
#' @export kdollar
kdollar <- function(x, dp = 0, form = "f", ...){
  paste0(ifelse(x < 0, "-", ""), "$", formatC(abs(x)/1e3, digits = dp, format = form, big.mark = ",", ...), "k")
}

#' Label in thousands
#'
#' A modification of the \code{\link[scales]{comma}} function from \code{scales}, which
#' is more appropriate for plots of large values
#' @param x A numeric amount
#'
#' @return A character representation of x in thousands of dollars
#'
#' @examples
#' kcomma(10000)
#' kcomma(c(2000, 1500000))
#' @export kcomma
kcomma <- function(x, dp = 0, form = "f", ...){
  paste0(ifelse(x < 0, "-", ""), formatC(abs(x)/1e3, digits = dp, format = form, big.mark = ",", ...), "k")
}

#' Convert numeric values to percentages
#'
#' A modification of the \code{\link[scales]{percent}} function from \code{scales}, which
#' gives better control over the decimal places displayed
#' @param x numeric representation of a percentage
#' @param dp the number of decimal places to display in the output
#' @param form Same as the \code{format} parameter in \code{\link{formatC}}
#' @param ... Additional options for formatC
#'
#' @return A character representation of \code{x} as a percentage
#' @export percent
#'
#' @examples
#' percent(0.01)
#' percent(c(0.001, 0.23423452, 0.2, 1.124, 1213.34))
#'
#' Compare to scales implementation:
#' scales::percent(c(0.001, 0.23423452, 0.2, 1.124, 1213.34))
percent <- function(x, dp = 1, form = "f", ...){
  paste0(formatC(x * 100, digits = dp, format = form, big.mark = ",", ...), "%")
}


#' Convert numeric values to percentages, with '+' indicating positive growth
#'
#' A modification of the \code{\link[scales]{percent}} function from \code{scales}, which
#' gives better control over the decimal places displayed
#' @param x numeric representation of a percentage
#' @param dp the number of decimal places to display in the output
#' @param form Same as the \code{format} parameter in \code{\link{formatC}}
#' @param ... Additional options for formatC
#'
#' @return A character representation of \code{x} as a percentage
#' @export percent_change
#'
#' @examples
#' percent(0.01)
#' percent(c(0.001, 0.23423452, 0.2, 1.124, 1213.34))
#'
#' Compare to scales implementation:
#' scales::percent(c(0.001, 0.23423452, 0.2, 1.124, 1213.34))
percent_change <- function(x, dp = 1, form = "f", ...){
  paste0(ifelse(x > 0, "+", ""), formatC(x * 100, digits = dp, format = form, big.mark = ",", ...), "%")
}



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
#' @param base_dir The base directory of the project
#' @param data_name The name that the data object has. Defaults to "project name data.RData"
#'
#' @return imports the processed data into the global environment
#' @export data_import
data_import <- function(base_dir = getwd(), data_name = paste(gsub(".*/(?!$)|/$", "", base_dir, perl = TRUE), "data.RData")) {

  # Construct the data directory
  data_dir <- file.path(base_dir, "data")

  # When was the processing script last updated?
  processing_time <- file.info(file.path(base_dir, "R", paste("1", gsub(".*/(?!$)|/$", "", base_dir, perl = TRUE), "processing.R")))$mtime

  # When was the data last updated?
  data_time <- file.info(file.path(data_dir, "processed", paste(gsub(".*/(?!$)|/$", "", base_dir, perl = TRUE), "data.RData")))$mtime


  # Import the data
  load(file.path(data_dir, "processed", data_name), envir = .GlobalEnv)

  warning(paste("Data processing script last updated", processing_time))

  warning(paste("Data last updated", data_time))

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
parse_seqmonth <- function(x){
  if (unique(nchar(x)) != 6) {
    stop("You should only provide SEQMONTH attributes")
  }

  x_date <- as.Date(paste0(x, "01"), format = "%Y%m%d")
}
