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

  # Add the original values to the flextable for reference
  df_ft$vals <- df

  return(df_ft)
}


#' Create a dark-themed FlexTable
#'
#' A function to create a FlexTable object for use in MS Office type outputs via the ReporteRs package
#'
#' @param df The data which should populate the flextable
#' @param size Which size should the resulting FlexTable's text be?
#' @param padding How much padding should there be around the text?
#'
#' @export flextable_dark
#'
#' @examples
#' flextable_dark(head(cars))
flextable_dark <- function(df, size = 12, padding = size / 2) {
  # Create the FlexTable of the data
  df_ft <- ReporteRs::FlexTable(df, header.cell.props = ReporteRs::cellProperties(background.color = marketview::mvl_blue, padding.top = padding, padding.bottom = padding),
                                header.text.props = ReporteRs::textProperties(color = "white", font.size = size, font.family = "Calibri", font.weight = "bold"),
                                header.par.props = ReporteRs::parProperties(text.align = "center"),
                                body.cell.props = ReporteRs::cellProperties(padding.top = padding, padding.bottom = padding),
                                body.par.props = ReporteRs::parProperties(text.align = "center"),
                                body.text.props = ReporteRs::textProperties(color = marketview::mvl_blue, font.size = size, font.family = "Calibri")
  )

  # Add the 'zebra' cell fill
  df_ft <- ReporteRs::setZebraStyle(df_ft, odd = "grey95", even = marketview::mvl_half_grey)

  # Set the cell borders
  df_ft <- ReporteRs::setFlexTableBorders(df_ft, inner.vertical = ReporteRs::borderProperties( color = "white", style= "solid"),
                                          inner.horizontal = ReporteRs::borderProperties( color = "white", style = "solid" ),
                                          outer.vertical = ReporteRs::borderProperties( color = "white", style = "solid" ),
                                          outer.horizontal = ReporteRs::borderProperties( color = "white", style = "solid"))

  # Add the original values to the flextable for reference
  df_ft$vals <- df

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
flextable_row_highlight <- function(ft, rows, cols = 1:ft$numcol){
  ft[rows, cols, side = "top"] = ReporteRs::borderProperties(color = "gold", width = 2)
  ft[rows, cols, side = "bottom"] = ReporteRs::borderProperties(color = "gold", width = 2)
  ft[rows, min(cols), side = "left"] = ReporteRs::borderProperties(color = "gold", width = 2)
  ft[rows, max(cols), side = "right"] = ReporteRs::borderProperties(color = "gold", width = 2)

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
#' @export flextable_cell_bold
#'
#' @examples
#' df_ex <- head(cars)
#' ft_ex <- flextable_teal(df_ex)
#' ft_ex <- flextable_cell_bold(ft_ex, which(df_ex$speed > 7), "speed", "forestgreen")

flextable_cell_bold <- function(ft, rows, cols, colour = marketview::mvl_text){
  existing_props <- ReporteRs::textProperties(font.size = ft$body.text.props[]$font.size,
                                              font.weight = ft$body.text.props[]$font.weight,
                                              font.style = ft$body.text.props[]$font.style,
                                              underlined = ft$body.text.props[]$underlined,
                                              font.family = ft$body.text.props[]$font.family,
                                              vertical.align = ft$body.text.props[]$vertical.align)

  ft[rows, cols] <- ReporteRs::chprop(existing_props, font.weight = "bold", color = colour)

  return(ft)
}



#' Create a black-themed FlexTable
#'
#' A function to create a FlexTable object for use in MS Office type outputs via the ReporteRs package
#'
#' @param df The data which should populate the flextable
#' @param size Which size should the resulting FlexTable's text be?
#' @param padding How much padding should there be around the text?
#'
#' @export flextable_black
#'
#' @examples
#' flextable_black(head(cars))
flextable_black <- function(df, size = 12, padding = size / 2) {
  # Create the FlexTable of the data
  df_ft <- ReporteRs::FlexTable(df, header.cell.props = ReporteRs::cellProperties(background.color = "black", padding.top = padding, padding.bottom = padding),
                                header.text.props = ReporteRs::textProperties(color = "white", font.size = size, font.family = "Helvetica Neue", font.weight = "bold"),
                                header.par.props = ReporteRs::parProperties(text.align = "center"),
                                body.cell.props = ReporteRs::cellProperties(padding.top = padding, padding.bottom = padding),
                                body.par.props = ReporteRs::parProperties(text.align = "center"),
                                body.text.props = ReporteRs::textProperties(color = marketview::mvl_text, font.size = size, font.family = "Helvetica Neue")
  )

  # Add the 'zebra' cell fill
  df_ft <- ReporteRs::setZebraStyle(df_ft, odd = "grey90", even = marketview::mvl_half_grey)

  # Set the cell borders
  df_ft <- ReporteRs::setFlexTableBorders(df_ft, inner.vertical = ReporteRs::borderProperties( color="white", style="solid"),
                                          inner.horizontal = ReporteRs::borderProperties( color = "white", style = "solid" ),
                                          outer.vertical = ReporteRs::borderProperties( color = "white", style = "solid" ),
                                          outer.horizontal = ReporteRs::borderProperties( color = "white", style = "solid"))

  # Add the original values to the flextable for reference
  df_ft$vals <- df

  return(df_ft)
}


#' Create a leaf-themed FlexTable
#'
#' A function to create a FlexTable object for use in MS Office type outputs via the ReporteRs package
#'
#' @param df The data which should populate the flextable
#' @param size Which size should the resulting FlexTable's text be?
#' @param padding How much padding should there be around the text?
#'
#' @export flextable_leaf
#'
#' @examples
#' flextable_leaf(head(cars))
flextable_leaf <- function(df, size = 12, padding = size / 2) {
  # Create the FlexTable of the data
  df_ft <- ReporteRs::FlexTable(df, header.cell.props = ReporteRs::cellProperties(background.color = marketview::mvl_leaf, padding.top = padding, padding.bottom = padding),
                                header.text.props = ReporteRs::textProperties(color = "white", font.size = size, font.family = "Helvetica Neue", font.weight = "bold"),
                                header.par.props = ReporteRs::parProperties(text.align = "center"),
                                body.cell.props = ReporteRs::cellProperties(padding.top = padding, padding.bottom = padding),
                                body.par.props = ReporteRs::parProperties(text.align = "center"),
                                body.text.props = ReporteRs::textProperties(color = marketview::mvl_text, font.size = size, font.family = "Helvetica Neue")
  )

  # Add the 'zebra' cell fill
  df_ft <- ReporteRs::setZebraStyle(df_ft, odd = grDevices::rgb(236, 241, 233, maxColorValue = 255), even = grDevices::rgb(214, 226, 207, maxColorValue = 255))

  # Set the cell borders
  df_ft <- ReporteRs::setFlexTableBorders(df_ft, inner.vertical = ReporteRs::borderProperties( color="white", style="solid"),
                                          inner.horizontal = ReporteRs::borderProperties( color = "white", style = "solid" ),
                                          outer.vertical = ReporteRs::borderProperties( color = "white", style = "solid" ),
                                          outer.horizontal = ReporteRs::borderProperties( color = "white", style = "solid"))

  # Add the original values to the flextable for reference
  df_ft$vals <- df

  return(df_ft)
}


#' Create a FlexTable with Multiple Header Rows
#'
#' Format and print a joined FlexTable
#'
#' @param df A \code{data.frame} of values
#' @param pr Pseudo-rows: Indices of the header cells in df to be merged
#' @param hr Header Rows: Indices of the rows to apply header formatting
#' @param size Font size in pt
#' @param padding Cell padding
#'
#' @return df_ft A FlexTable of the df with formatting
#' @export flextable_leaf_joined
flextable_leaf_joined <- function(df, pr, hr,
                                  size = 12, padding = size/2){

  #Create FT
  df_ft <- ReporteRs::FlexTable(df,
                                header.columns = F,
                                body.cell.props = ReporteRs::cellProperties(padding.top = padding,
                                                                            padding.bottom = padding), body.par.props = ReporteRs::parProperties(text.align = "center"),
                                body.text.props = ReporteRs::textProperties(color = marketview::mvl_text,
                                                                            font.size = size, font.family = "Helvetica Neue"))

  df_ft <- ReporteRs::setZebraStyle(df_ft, odd = grDevices::rgb(236,
                                                                241, 233, maxColorValue = 255), even = grDevices::rgb(214,
                                                                                                                      226, 207, maxColorValue = 255))

  #If necessary join heading rows
  if(!is.null(pr)){
    df_ft %>%
      ReporteRs::spanFlexTableColumns(i = pr, runs = as.character(df[pr,]))
  }

  #Format the heading rows
  df_ft[i = hr] <- ReporteRs::cellProperties(background.color = marketview::mvl_leaf,
                                             padding.top = padding, padding.bottom = padding)

  df_ft[i = hr] <- ReporteRs::textProperties(color = "white",
                                             font.size = size, font.family = "Helvetica Neue", font.weight = "bold")

  #Format borders
  df_ft <- ReporteRs::setFlexTableBorders(df_ft,
                                          inner.vertical = ReporteRs::borderProperties(color = "white", style = "solid"),
                                          inner.horizontal = ReporteRs::borderProperties(color = "white", style = "solid"),
                                          outer.vertical = ReporteRs::borderProperties(color = "white", style = "solid"),
                                          outer.horizontal = ReporteRs::borderProperties(color = "white", style = "solid"))

  # Add the original values to the flextable for reference
  df_ft$vals <- df


  return(df_ft)
}



#' Highlight Negative Cell Values in Red
#'
#' A function to highlight any negative values in a FlexTable for given column names.
#' This should work regardless of the formatting - character values such as those returned by \code{marketview::dollar()} or
#' \code{marketview::percent()} will be dealt with if I've done this right.
#'
#' @param ftbl A FlexTable object to format
#' @param col_names The names of the columns in which to highlight the negatives
#'
#' @return ftbl The original FlexTable with the formatting applied
#' @export flextable_negatives
flextable_negatives <- function(ftbl, col_names) {

  # Get the original data.frame from the flextable object
  df <- as.data.frame(ftbl$vals)

  for(column in col_names) {
    # Get the specified columns
    col_ind <- which(ftbl$col_id == column)
    col_vals <- df[, col_ind]

    # Get the values as numerics
    col_vals <-  gsub("[^-\\.0-9]", "", col_vals)
    col_vals <- as.numeric(col_vals)

    # Find any values that are less than zero
    neg_ind <- which(col_vals < 0)

    # Highlight the negative values in the flextable
    ftbl <- marketview::flextable_cell_bold(ftbl, neg_ind, col_ind, marketview::mvl_red)
  }
  return(ftbl)
}
