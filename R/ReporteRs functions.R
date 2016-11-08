# ReporteRs functions.R

#  ------------------------------------------------------------------------

# Collection of functions that will make collating results with the
# ReporteRs package a bit more straightforward

#  ------------------------------------------------------------------------

#' Create a teal-themed FlexTable
#'
#' A function to create a FlexTable object for use in MS Office type outputs via the ReporteRs package
#'
#' @param df The data which should populate the flextable
#' @param size Which size should the resulting FlexTable's text be?
#' @param padding How much padding should there be around the text?
#'
#' @export flextable_teal
#'
#' @examples
#' flextable_teal(head(cars))
flextable_teal <- function(df, size = 12, padding = size / 2) {
  # Create the FlexTable of the data
  df_ft <- ReporteRs::FlexTable(df, header.cell.props = ReporteRs::cellProperties(background.color = marketview::mvl_teal, padding.top = padding, padding.bottom = padding),
                                header.text.props = ReporteRs::textProperties(color = "white", font.size = size, font.family = "Calibri", font.weight = "bold"),
                                header.par.props = ReporteRs::parProperties(text.align = "center"),
                                body.cell.props = ReporteRs::cellProperties(padding.top = padding, padding.bottom = padding),
                                body.par.props = ReporteRs::parProperties(text.align = "center"),
                                body.text.props = ReporteRs::textProperties(color = "black", font.size = size, font.family = "Calibri")
  )

  # Add the 'zebra' cell fill
  df_ft <- ReporteRs::setZebraStyle(df_ft, odd = "white", even = marketview::mvl_half_teal)

  # Set the cell borders
  df_ft <- ReporteRs::setFlexTableBorders(df_ft, inner.vertical = ReporteRs::borderProperties( color="white", style="solid"),
                                          inner.horizontal = ReporteRs::borderProperties( color = "white", style = "solid" ),
                                          outer.vertical = ReporteRs::borderProperties( color = "white", style = "solid" ),
                                          outer.horizontal = ReporteRs::borderProperties( color = "white", style = "solid"))

  return(df_ft)
}


#' Highlight given rows of a flextable
#'
#' A function to highlight particular rows of a FlexTable with gold borders
#'
#' @param ft The flextable object to highlight
#' @param rows The index of the rows that should be highlighted
#'
#' @return ft A modified FlexTable with the specified rows highlighted in gold
#' @export flextable_row_highlight
#'
#' @examples
#' df_ex <- head(cars)
#' ft_ex <- flextable_teal(df_ex)
#' ft_ex <- flextable_row_highlight(ft_ex, which(df_ex$dist == max(df_ex$dist)))
flextable_row_highlight <- function(ft, rows){
  ft[rows, side = "top"] = ReporteRs::borderProperties(color = "gold", width = 2)
  ft[rows, side = "bottom"] = ReporteRs::borderProperties(color = "gold", width = 2)
  ft[rows, 1, side = "left"] = ReporteRs::borderProperties(color = "gold", width = 2)
  ft[rows, ft$numcol, side = "right"] = ReporteRs::borderProperties(color = "gold", width = 2)

  return(ft)
}

#' Bold cells of a flextable
#'
#' A function to highlight particular cells of a FlexTable with bold text and colour
#'
#' @param ft The flextable object to highlight
#' @param rows The index of the rows that should be highlighted
#' @param cols The columns of the cells that should be highlighted
#' @param colour The colour that the bold text should have
#'
#' @return ft A modified FlexTable with the specified cells highlighted with bold text in the given colour
#' @export flextable_row_highlight
#'
#' @examples
#' df_ex <- head(cars)
#' ft_ex <- flextable_teal(df_ex)
#' ft_ex <- flextable_cell_bold(ft_ex, which(df_ex$speed > 7), "speed", "forestgreen")

flextable_cell_bold <- function(ft, rows, cols, colour = "black"){
  existing_props <- ReporteRs::textProperties(font.size = ft$body.text.props[]$font.size,
                                              font.weight = ft$body.text.props[]$font.weight,
                                              font.style = ft$body.text.props[]$font.style,
                                              underlined = ft$body.text.props[]$underlined,
                                              font.family = ft$body.text.props[]$font.family,
                                              vertical.align = ft$body.text.props[]$vertical.align)

  ft[rows, cols] <- ReporteRs::chprop(existing_props, font.weight = "bold", color = colour)

  return(ft)
}
