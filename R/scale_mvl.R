# pal_mvl.R
# -------------------------------------------------------------------------
# A script detailing the function used to create colour palettes in line
# with the Marketview colour guides
# -------------------------------------------------------------------------
# Created by Bert on 14-03-2017
# -------------------------------------------------------------------------
# Last edited by Bert on 14-03-2017
# -------------------------------------------------------------------------

#' Marketview Palette
#'
#' @description
#' The \code{Marketview} palettes provide sequential, diverging and qualitative
#' colour schemes in line with the Marketview branding guides.
#'
#' @section Palettes:
#' The following palettes are available for use with these scales:
#' \describe{
#'   \item{Diverging}{LeafPlum, TealRed, RedStone, SkyStone, PlumStone, LeafStone, RoyalStone}
#'   \item{Qualitative}{mvl, mvl_classic, mcd, mcd2, fssi}
#'   \item{Sequential}{Leaf, Plum, Teal, Blue, Sky, Citrus, Stone,
#'      Navy, Orange, Red, Green, Purple, Highlight}
#' }
#' If there are others that you want to be implemented, let me (Bert) know
#'
#' @param palette Name of the palette to use
#' @param direction Set the order of the colours in the scale
#'
#' @export pal_mvl
pal_mvl <- function(palette = "Leaf", direction = 1, highlight = mvl_leaf) {

  if (!(palette %in% c("Leaf", "Plum", "Teal", "Blue", "Sky", "Citrus", "Stone",
                       "Navy", "Orange", "Red", "Green", "Purple", "Highlight",
                       "LeafPlum", "TealRed", "RedStone", "SkyStone", "PlumStone", "LeafStone", "RoyalStone",
                       "mcd", "mvl", "mcd2", "mvl_classic", "fssi"))) {
    stop("Palette not available. See ?scale_mvl for a list of palettes that are currently implemented. Email Bert if you have any suggestions.")
  }

  # Single colour palettes ----
  if (palette %in% c("Leaf", "Plum", "Teal", "Blue", "Sky", "Citrus", "Stone",
                     "Navy", "Orange", "Red", "Green", "Purple", "Highlight")) {

    # Match the name to the base colour ----
    pal_ref <- data.frame(
      pal_name = c("Leaf", "Plum", "Teal", "Blue", "Sky", "Citrus", "Stone",
                   "Navy", "Orange", "Red", "Green", "Purple"),
      pal_col = c(marketview::mvl_leaf, marketview::mvl_plum, marketview::mvl_teal,
                  marketview::mvl_blue, marketview::mvl_sky, marketview::mvl_citrus,
                  marketview::mvl_stone, marketview::mvl_navy, marketview::mvl_orange,
                  marketview::mvl_red, marketview::mvl_green, marketview::mvl_purple)
    )

    top_col <- pal_ref[pal_ref$pal_name == palette, "pal_col"]

    # Construct the palette function ----
    pal_out <- function(n, dirn = direction){

      if (dirn == 1) {
        dir_vec <- 1:n
      } else if (dirn == -1) {
        dir_vec <- n:1
      } else stop("direction should be -1 or 1")

      if (palette == "Highlight") {
        c(highlight, colorRampPalette(c(mvl_stone, "grey75"))(n))[dir_vec]
      } else colorRampPalette(c(top_col, "white"))(n + 1)[dir_vec]
    }

  }

  # Diverging Scales ----
  if (palette %in% c("LeafPlum", "TealRed", "RedStone", "SkyStone", "PlumStone", "LeafStone", "RoyalStone")) {

    # Match the palette name to the colours ----
    pal_ref <- data.frame(
      pal_name = c("LeafPlum", "TealRed", "RedStone", "SkyStone", "PlumStone", "LeafStone", "RoyalStone"),
      pal_min = c(marketview::mvl_leaf, marketview::mvl_teal, marketview::mvl_red,
                  marketview::mvl_sky, marketview::mvl_plum, marketview::mvl_leaf, marketview::mvl_royal),
      pal_max = c(marketview::mvl_plum, marketview::mvl_red, marketview::mvl_stone,
                  marketview::mvl_stone, marketview::mvl_stone, marketview::mvl_stone, marketview::mvl_stone)
    )

    min_col <- pal_ref[pal_ref$pal_name == palette, "pal_min"]
    max_col <- pal_ref[pal_ref$pal_name == palette, "pal_max"]

    # construct the palette function ----
    pal_out <- function(n, dirn = direction){

      n_new <- 2 * floor(n / 2) + 1

      if (dirn == 1) {
        dir_vec <- 1:n_new
      } else if (dirn == -1) {
        dir_vec <- n_new:1
      } else stop("direction should be -1 or 1")


      if (n %% 2 == 0) {
        # take the central value out if getting an even number of colours ----
        dir_vec <- dir_vec[-ceiling(length(dir_vec) / 2)]
      }


      grDevices::colorRampPalette(c(min_col, "#f7f7f7", max_col))(n_new)[dir_vec]
    }

  }


  # Qualitative Scales ----

  if (palette %in% c("mcd", "mvl", "mcd2", "mvl_classic", "fssi")) {

    pal_ref <- dplyr::tibble(
      pal_name = c("mcd", "mvl", "mcd2", "mvl_classic", "fssi"),
      vals = list(c(marketview::mvl_red, marketview::mvl_green, marketview::mvl_orange,
                    marketview::mvl_teal, marketview::mvl_navy, marketview::mvl_purple,
                    marketview::mvl_hay),
                  c(marketview::mvl_leaf, marketview::mvl_plum, marketview::mvl_sky,
                    marketview::mvl_citrus, marketview::mvl_fire, marketview::mvl_royal2,
                    marketview::mvl_stone),
                  c(marketview::mvl_red, marketview::mvl_orange,
                    marketview::mvl_teal, marketview::mvl_navy, marketview::mvl_purple,
                    marketview::mvl_hay),
                  c(marketview::mvl_teal, marketview::mvl_navy,
                    marketview::mvl_royal, marketview::mvl_purple, marketview::mvl_half_purple,
                    marketview::mvl_half_grey),
                  c(marketview::mvl_red, marketview::mvl_orange, marketview::mvl_teal,
                    marketview::mvl_green, marketview::mvl_navy, marketview::mvl_purple,
                    marketview::mvl_hay))
    )

    scale_vals <- as.character(
      unlist(
        pal_ref[pal_ref$pal_name == palette, "vals"]
      )
    )

    pal_out <- function(n, dirn = direction) {
      if (dirn == 1) {
        dir_vec <- 1:n
      } else if (dirn == -1) {
        dir_vec <- n:1
      } else stop("direction should be -1 or 1")

      if (n > length(scale_vals)) {
        warning(paste("n too large, allowed maximum for palette", palette, "is", length(scale_vals),
                "\nReturning the palette you asked for with that many colors\n"))
       }
      scale_vals <- scale_vals[dir_vec]
      scale_vals[is.na(scale_vals)] <- scales::grey_pal()(length(scale_vals[is.na(scale_vals)]))
      return(scale_vals)
    }
  }

  # Return the palette function ----
  return(pal_out)
}


#' Marketview Colour Scales
#'
#' @description
#' The \code{Marketview} scales provide sequential, diverging and qualitative
#' colour schemes in line with the Marketview branding guides.
#'
#' @section Palettes:
#' The following palettes are available for use with these scales:
#' \describe{
#'   \item{Diverging}{LeafPlum, TealRed, RedStone, SkyStone, PlumStone, LeafStone, RoyalStone}
#'   \item{Qualitative}{mvl, mcd, mcd2, mvl_classic, fssi}
#'   \item{Sequential}{Leaf, Plum, Teal, Blue, Sky, Citrus, Stone,
#'      Navy, Orange, Red, Green, Purple, Highlight}
#' }
#' If there are others that you want to be implemented, let me (Bert) know
#'
#'
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}} to control name, limits, breaks, labels and so forth.
#' @inheritParams pal_mvl
#'
#' @name scale_mvl
NULL

#' @rdname scale_mvl
#' @export scale_colour_mvl
scale_colour_mvl <- function(..., palette = "mvl", direction = 1, highlight = mvl_leaf, guide = ggplot2::guide_legend(byrow = TRUE)) {
  ggplot2::discrete_scale("colour", "brewer", pal_mvl(palette, direction, highlight), guide = guide, ...)
}


#' @rdname scale_mvl
#' @export scale_fill_mvl
scale_fill_mvl <- function(..., palette = "mvl", direction = 1, highlight = mvl_leaf, guide = ggplot2::guide_legend(byrow = TRUE)) {
  ggplot2::discrete_scale("fill", "brewer", pal_mvl(palette, direction, highlight), guide = guide, ...)
}


#' @rdname scale_mvl
#' @export scale_colour_mvlc
scale_colour_mvlc <- function(..., palette = "Leaf", direction = -1, values = NULL, na.value = "grey50", guide = "colourbar") {
  # warn about using a qualitative palette to generate the gradient
  if (palette %in% c("mcd", "mvl")) {
    warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", call. = FALSE)
  }
  ggplot2::continuous_scale("colour", "distiller",
                   scales::gradient_n_pal(pal_mvl(palette, direction)(6), values), na.value = na.value, guide = guide, ...)
  # NB: 6 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
}

#' @rdname scale_mvl
#' @export scale_fill_mvlc
scale_fill_mvlc <- function(..., palette = "Leaf", direction = -1, values = NULL, na.value = "grey50", guide = "colourbar") {
  # warn about using a qualitative palette to generate the gradient
  if (palette %in% c("mcd", "mvl")) {
    warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", call. = FALSE)
  }
  ggplot2::continuous_scale("fill", "distiller",
                   scales::gradient_n_pal(pal_mvl(palette, direction)(6), values), na.value = na.value, guide = guide, ...)
}
