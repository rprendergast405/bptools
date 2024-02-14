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
pal_mvl <- function(palette = "Sky", direction = 1, highlight = mvl_leaf) {

  if (!(palette %in% c("Leaf", "Plum", "Teal", "Blue", "Sky", "Citrus", "Stone",
                       "Navy", "Orange", "Red", "Green", "Purple", "Highlight",
                       "LeafPlum", "TealRed", "RedStone", "SkyStone", "PlumStone", "LeafStone", "RoyalStone",
                       "mcd", "mvl", "mcd2", "mvl_classic", "fssi", "mvl2"))) {
    stop("Palette not available. See ?scale_mvl for a list of palettes that are currently implemented. Email Bert if you have any suggestions.")
  }

  # Single colour palettes ----
  if (palette %in% c("Leaf", "Plum", "Teal", "Blue", "Sky", "Citrus", "Stone",
                     "Navy", "Orange", "Red", "Green", "Purple", "Highlight")) {

    # Match the name to the base colour ----
    # pal_ref <- data.frame(
    #   pal_name = c("Leaf", "Plum", "Teal", "Blue", "Sky", "Citrus", "Stone",
    #                "Navy", "Orange", "Red", "Green", "Purple"),
    #   pal_col = c(bptools::mvl_leaf, bptools::mvl_plum, bptools::mvl_teal,
    #               bptools::mvl_blue, bptools::mvl_sky, bptools::mvl_citrus,
    #               bptools::mvl_stone, bptools::mvl_navy, bptools::mvl_orange,
    #               bptools::mvl_red, bptools::mvl_green, bptools::mvl_purple)
    # )

    # pal_ref <- data.frame(
    #   pal_name = c("Leaf", "Plum", "Teal", "Blue", "Sky", "Citrus", "Stone",
    #                "Navy", "Orange", "Red", "Green", "Purple"),
    #   pal_top = c("#415c24", "#641b63", bptools::mvl_teal,
    #               bptools::mvl_blue, "#004459", bptools::mvl_citrus,
    #               bptools::mvl_stone, bptools::mvl_navy, bptools::mvl_orange,
    #               bptools::mvl_red, bptools::mvl_green, bptools::mvl_purple),
    #   pal_bottom = c("#d9eac8", "#efc3ee", bptools::mvl_teal,
    #                  bptools::mvl_blue, "#b3edff", bptools::mvl_citrus,
    #                  bptools::mvl_stone, bptools::mvl_navy, bptools::mvl_orange,
    #                  bptools::mvl_red, bptools::mvl_green, bptools::mvl_purple)
    # )

    pal_ref <- tibble::tibble(
      pal_name = c("Leaf", "Plum", "Sky",
                   "Citrus", "Stone", "Navy", "Orange",
                   "Red", "Green", "Purple"),
      pal_vec = list(c("#415c24", "#5b8132", "#78aa42", "#8ebf5a", "#a7cd7e", "#d9eac8"),
                     c("#641b63", "#91278f", "#ce4acc", "#d972d7", "#e49be3", "#efc3ee"),
                     c("#004459", "#006b8c", "#0092bf", "#00B9F2", "#80e1ff", "#b3edff"),
                     c(bptools::mvl_citrus, "#f7f7f7"),
                     c(bptools::mvl_stone, "#f7f7f7"),
                     c(bptools::mvl_navy, "#f7f7f7"),
                     c(bptools::mvl_orange, "#f7f7f7"),
                     c(bptools::mvl_red, "#f7f7f7"),
                     c(bptools::mvl_green, "#f7f7f7"),
                     c(bptools::mvl_purple, "#f7f7f7")
      )
    )


    # top_col <- pal_ref[pal_ref$pal_name == palette, "pal_top"]
    # bottom_col <- pal_ref[pal_ref$pal_name == palette, "pal_bottom"]
    pal_vec <- dplyr::pull(pal_ref[pal_ref$pal_name == palette, ], pal_vec)

    # Construct the palette function ----
    pal_out <- function(n, dirn = direction){

      if (dirn == 1) {
        dir_vec <- 1:n
      } else if (dirn == -1) {
        dir_vec <- n:1
      } else stop("direction should be -1 or 1")

      if (palette == "Highlight") {
        out <- c(highlight, colorRampPalette(c(mvl_stone, "grey75"))(n))[dir_vec]
      } else if (palette %in% c("Leaf", "Plum", "Sky")) {
        out <- colorRampPalette(pal_vec[[1]])(n)[dir_vec]
      } else out <- colorRampPalette(pal_vec[[1]])(n + 1)[dir_vec]

      # Failed attempt to destuarate colour values

      # desat <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(out))
      # out <- hsv(desat[1,], desat[2,], desat[3,])

      return(out)
    }

  }

  # Diverging Scales ----
  if (palette %in% c("LeafPlum", "TealRed", "RedStone", "SkyStone", "PlumStone", "LeafStone", "RoyalStone")) {

    # Match the palette name to the colours ----
    pal_ref <- data.frame(
      pal_name = c("LeafPlum", "TealRed", "RedStone", "SkyStone", "PlumStone", "LeafStone", "RoyalStone"),
      pal_min = c(bptools::mvl_leaf, bptools::mvl_teal, bptools::mvl_red,
                  bptools::mvl_sky, bptools::mvl_plum, bptools::mvl_leaf, bptools::mvl_royal),
      pal_max = c(bptools::mvl_plum, bptools::mvl_red, bptools::mvl_stone,
                  bptools::mvl_stone, bptools::mvl_stone, bptools::mvl_stone, bptools::mvl_stone)
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


      out <- grDevices::colorRampPalette(c(min_col, "#f7f7f7", max_col))(n_new)[dir_vec]

      # failed attempt to desturate values
      # desat <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(out))
      # out <- hsv(desat[1,], desat[2,], desat[3,])

      return(out)

    }

  }


  # Qualitative Scales ----

  if (palette %in% c("mcd", "mvl", "mcd2", "mvl_classic", "fssi", "mvl2")) {

    pal_ref <- dplyr::tibble(
      pal_name = c("mcd", "mvl2", "mcd2", "mvl_classic", "fssi"),
      vals = list(c(bptools::mvl_orange, bptools::mvl_navy, bptools::mvl_red,
                    bptools::mvl_green, bptools::mvl_teal, bptools::mvl_purple,
                    bptools::mvl_hay),
                  c(bptools::mvl_leaf, bptools::mvl_plum, bptools::mvl_sky,
                    bptools::mvl_citrus, bptools::mvl_fire, bptools::mvl_royal2,
                    bptools::mvl_stone),
                  c(bptools::mvl_red, bptools::mvl_orange,
                    bptools::mvl_teal, bptools::mvl_navy, bptools::mvl_purple,
                    bptools::mvl_hay),
                  c(bptools::mvl_teal, bptools::mvl_navy,
                    bptools::mvl_royal, bptools::mvl_purple, bptools::mvl_half_purple,
                    bptools::mvl_half_grey),
                  c(bptools::mvl_red, bptools::mvl_orange, bptools::mvl_teal,
                    bptools::mvl_green, bptools::mvl_navy, bptools::mvl_purple,
                    bptools::mvl_hay))
    )

    mvl_ref <- dplyr::tibble(
      num = 1:12,
      vals = list(c("#00b9f2"),
                  c("#00b9f2", "#91278f"),
                  c("#00b9f2", "#003a4d", "#91278f"),
                  c("#00b9f2", "#003a4d", "#d96dd4", "#91278f"),
                  c("#00b9f2", "#003a4d", "#d96dd4", "#91278f", "#58595b"),
                  c("#00b9f2", "#003a4d", "#d96dd4", "#91278f", "#d7df23", "#58595b"),
                  c("#00b9f2", "#006fa4", "#003a4d", "#d96dd4", "#91278f", "#d7df23", "#58595b"),
                  c("#00b9f2", "#006fa4", "#003a4d", "#ffb3ff", "#d96dd4", "#91278f", "#d7df23", "#58595b"),
                  c("#00b9f2", "#006fa4", "#003a4d", "#ffb3ff", "#d96dd4", "#91278f", "#d7df23", "#58595b", "#b2abf2"),
                  c("#00b9f2", "#006fa4", "#003a4d", "#ffb3ff", "#d96dd4", "#91278f", "#d7df23", "#58595b", "#b2abf2", "#6460a1"),
                  c("#00b9f2", "#006fa4", "#003a4d", "#ffb3ff", "#d96dd4", "#91278f", "#d7df23", "#58595b", "#b2abf2", "#6460a1", "#b8b9bb"),
                  c("#00b9f2", "#006fa4", "#003a4d", "#ffb3ff", "#d96dd4", "#91278f", "#d7df23", "#58595b", "#b2abf2", "#6460a1", "#b8b9bb", "#8cc63f"))
    )



    pal_out <- function(n, dirn = direction) {

      if (palette == "mvl") {
        scale_vals <- as.character(
          unlist(
            mvl_ref[mvl_ref$num == n, "vals"]
          )
        )
      } else {
        scale_vals <- as.character(
          unlist(
            pal_ref[pal_ref$pal_name == palette, "vals"]
          )
        )
      }

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

      # failed attempt to desaturate the palette
      # desat <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(scale_vals))
      # out <- hsv(desat[1,], desat[2,], desat[3,])

      out <- scale_vals
      return(out)
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
scale_colour_mvl <- function(..., palette = "mvl", direction = 1, highlight = mvl_leaf,
                             guide = ggplot2::guide_legend(byrow = TRUE)) {
  ggplot2::discrete_scale("colour", "brewer", pal_mvl(palette, direction, highlight), guide = guide, ...)
}


#' @rdname scale_mvl
#' @export scale_fill_mvl
scale_fill_mvl <- function(..., palette = "mvl", direction = 1, highlight = mvl_leaf,
                           guide = ggplot2::guide_legend(byrow = TRUE)) {
  ggplot2::discrete_scale("fill", "brewer", pal_mvl(palette, direction, highlight), guide = guide, ...)
}


#' @rdname scale_mvl
#' @export scale_colour_mvlc
scale_colour_mvlc <- function(..., palette = "Leaf", direction = -1, values = NULL, na.value = "grey50",
                              guide = "colourbar") {
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
scale_fill_mvlc <- function(..., palette = "Leaf", direction = -1, values = NULL, na.value = "grey50",
                            guide = "colourbar") {
  # warn about using a qualitative palette to generate the gradient
  if (palette %in% c("mcd", "mvl")) {
    warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", call. = FALSE)
  }
  ggplot2::continuous_scale("fill", "distiller",
                   scales::gradient_n_pal(pal_mvl(palette, direction)(6), values), na.value = na.value, guide = guide, ...)
}
