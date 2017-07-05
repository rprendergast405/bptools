# mvl_functions.R
# -------------------------------------------------------------------------
# A script containing some misc day-to-day functions used in Marketview
# reporting
# -------------------------------------------------------------------------
# Created by Bert on 05-07-2016
# -------------------------------------------------------------------------
# Last edited by Bert on 12-07-2016
# -------------------------------------------------------------------------


#' Save a ggplot object.
#'
#' A function to save a plot using dimensions and resolution that are
#' suitable for a Marketview report.
#'
#' @param p A ggplot object
#' @param write_dir The directory in which the figure should be saved
#' @param name A character name that the saved object will have
#' @param fig_num A figure number that preceeds the name
#' @param shape The shape of the png ("square" for a figure in ppt with the text to the side, and "wide" for a figure in ppt with the text underneath)
#' @param res The resolution of the png
#' @param mvl_footer Should a Marketview footer image be included?
#' @param ... if 'shape'
#'
#' @return Saves the plot to file
#' @export save_plot

save_plot <- function(p,
                      write_dir,
                      name,
                      fig_num = 1,
                      shape = "square",
                      res = 360,
                      mvl_footer = FALSE,
                      ...) {
  if (shape == "square") {
    height = 700*4
    width = 850*4
  } else if (shape == "wide") {
    height = (11.07*35/0.77)*4
    width = (23.92*35/0.77)*4
  }
  figure_name <- file.path(write_dir, paste0(fig_num, "_", name, ".png"))
  png(filename = figure_name,
      height = height,
      width = width,
      res = res)

  if (mvl_footer) {
    print(p +
            theme(plot.margin = unit(c(.1, .1, .5, .1), units = 'in')))
    make_footnote()
  } else  print(p)

  dev.off()
}

#' Save of PNG for PowerPoint.
#'
#' Saves a ggplot object as a PNG that is suitable for a Marketview
#' report/presentation in ppt.
#'
#' @param p A ggplot object
#' @param filename The file_directory/name that the saved png will take
#' @param w The width of the saved png
#' @param h The height of the saved png
#' @param fam Font family to use
#' @param pm What margins should the saved plot have?
#' @param mvl_foot Should a Marketview footnote be included?
#' @param mvl_foot_text Which text should the footnote include?
#' @param mvl_foot_colour Which colour should the text be?
#' @param mvl_foot_size How large should the footnote be?
#'
#' @return Saves the plot to file
#' @export ppt_png
ppt_png <- function(p,
                    filename,
                    w = 22,
                    h = 12,
                    fam = "Calibri",
                    pm = c(.1, .1, .25, .1),
                    mvl_foot = FALSE,
                    mvl_foot_text = "",
                    mvl_foot_colour = mvl_grey,
                    mvl_foot_size = .8){

  png(filename,
      width = w,
      height = h,
      units = 'cm',
      res = 300,
      type = 'cairo',
      family = fam)

  if (mvl_foot) {
    print(p +
            theme(plot.margin = unit(c(.1, .1, .5, .1), units = 'in')))
    make_footnote(footnote_text = mvl_foot_text, color = mvl_foot_colour, size = mvl_foot_size)
  } else print(p + theme(plot.margin = unit(pm, units = 'in')))

  dev.off()

}


#' Save data.frame to csv & xlsx.
#'
#' Saves copies of a data frame to a given directory.
#'
#' @param x The table to save
#' @param write_dir The directory in which the tables should be saved
#' @param name The name of the saved object
#' @param tab_num A number that preceeds the name
#'
#' @return Saves the table to file
#' @export save_table
save_table <- function(x,
                       write_dir,
                       name,
                       tab_num = 1,
                       xlsx = TRUE,
                       xls_name = "all_tables.xlsx"){

  x <- as.data.frame(x)

    write_csv(x, path = file.path(write_dir, paste0(tab_num, "_", name, ".csv")))

  if (xlsx) {
    write.xlsx(x, file = file.path(write_dir, xls_name), sheetName = name, append = tab_num > 1)

    if (tab_num == 1) warning("An existing .xlsx may have been overwritten.")
  }
}

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
