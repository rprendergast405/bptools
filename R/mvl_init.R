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
  library(marketview)
  library(mvldata)
  library(glue)
  library(ReporteRs)
  library(RcppRoll)


  # Set the plot theme -----
  theme_set(theme_mvl())

  # set colour palettes -----
  scale_colour_discrete <-
    partial(scale_colour_mvl, palette = "mcd")
  scale_colour_continuous <- scale_colour_mvlc
  scale_fill_discrete <- partial(scale_fill_mvl, palette = "mcd")
  scale_fill_continuous <- scale_fill_mvlc

  # make geom_col() reverse stack by default ----
  geom_col <- function(mapping = NULL,
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


  grDevices::windowsFonts(
    calb = "Calibri Bold",
    cal = "Calibri",
    hn = "Helvetica Neue",
    hnb = "Helvetica Neue Bold",
    cen = "Century Gothic"
  )

  # Set ReporteRs defaults
  options(
    "ReporteRs-default-font" = "Helvetica Neue",
    "ReporteRs-fontsize" = 12,
    "ReporteRs-locale.region" = "GB"
  )

  # Prevent print.data.frame from destroying your session ----
  print.data.frame <- function(x, ..., n = NULL, width = NULL) {
    x <- dplyr::as.tbl(x)
    print(x, ..., n = NULL, width = NULL)
  }
  # Set stringsAsFactors to FALSE
  options("stringsAsFactors" = FALSE,
          "max.print" = 200)

  library(httr)
  options(RCurlOptions = list(proxy = "proxy.private.marketview.co.nz:3128"))
  options(rsconnect.http = "rcurl")
  set_config(use_proxy(url = "proxy.private.marketview.co.nz", port = 3128))

}
