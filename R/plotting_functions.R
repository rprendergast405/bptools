# plotting_functions.R
# -------------------------------------------------------------------------
# A script containing some functions commonly used when producing figures
# for Marketview reporting
# -------------------------------------------------------------------------
# Created by Bert on 05-07-2016
# -------------------------------------------------------------------------
# Last edited by Bert on 07-07-2016
# -------------------------------------------------------------------------

# Add calibri fonts ----
grDevices::windowsFonts(calb = "Calibri Bold",
                        cal = "Calibri")

#' Marketview ggplot theme.
#'
#'A minimal theme for plots that looks okay in a Marketview ppt report.
#'
#' @param base_size The base size for fonts
#' @param base_family The base family for fonts
#'
#' @return A theme object to be added to ggplot objects
#' @export theme_mvl
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(data.frame(x = 1:5, y = 1:5)) + geom_point(aes(x, y))
#' p + theme_mvl()
theme_mvl <- function(base_size = 16, base_family = "calb") {
  ggplot2::`%+replace%`(ggplot2::theme_minimal(base_size = base_size, base_family = base_family),
                        ggplot2::theme(
                          text = ggplot2::element_text(family = base_family,
                                                       face = "plain",
                                                       colour = grDevices::rgb(33, 89, 104, maxColorValue = 255),
                                                       size = base_size,
                                                       hjust = 0.5,
                                                       vjust = 0.5,
                                                       angle = 0,
                                                       lineheight = 0.9,
                                                       margin = grid::unit(rep(0.1, 4), "cm"),
                                                       debug = FALSE),

                          axis.title.x = ggplot2::element_blank(),
                          axis.title.y = ggplot2::element_blank(),

                          legend.text = ggplot2::element_text(size = base_size - 2, face = "plain"),
                          legend.title = ggplot2::element_blank(),
                          legend.position = "top",
                          legend.direction = "horizontal",

                          panel.grid.major.x = ggplot2::element_blank(),

                          plot.title = ggplot2::element_text(size = base_size*1.5, hjust = 0),
                          plot.margin = grid::unit(c(0.1, 0, 1.2, 0.1), "cm")
                        )
  )
}

#' Marketview ggplot map theme.
#'
#'A theme for maps that looks okay in a Marketview ppt report.
#'Similar to \code{\link{theme_mvl}} but it has a blue background, no axis labels,
#'and a floating legend.
#'
#' @param base_size The base size for fonts
#' @param base_family The base family for fonts
#'
#' @return A theme object to be added to ggplot objects
#' @export theme_map
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(data.frame(x = 1:5, y = 1:5)) + geom_point(aes(x, y))
#' p + theme_map()
theme_map <- function(base_size = 12, base_family = "calb") {
  ggplot2::`%+replace%`(ggplot2::theme_minimal(base_size = base_size, base_family = base_family),
    ggplot2::theme(
      line = ggplot2::element_blank(),
      text = ggplot2::element_text(family = base_family,
                          face = "plain",
                          colour = rgb(33, 89, 104, maxColorValue = 255),
                          size = base_size,
                          hjust = 0.5,
                          vjust = 0.5,
                          angle = 0,
                          lineheight = 0.9,
                          margin = grid::unit(rep(0.1, 4), "cm"),
                          debug = FALSE),

      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),

      legend.background = ggplot2::element_rect(fill = grDevices::rgb(1, 1, 1, 0.65), colour = "white"),
      legend.text = ggplot2::element_text(face = "plain", family = "cal"),
      legend.title = ggplot2::element_blank(),
      legend.position = c(1, 1),
      legend.direction = "vertical",
      legend.justification = c(1, 1),

      strip.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = 'lightblue'),

      plot.title = ggplot2::element_text(size = ggplot2::rel(2), hjust = 0),
      plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    )
  )
}

#' Marketview ggplot map theme (minimal).
#'
#'A minimal theme for maps that is more suitable for faceted plots.
#'
#' @param base_size The base size for fonts
#' @param base_family The base family for fonts
#'
#' @return A theme object to be added to ggplot objects
#' @export theme_map_minimal
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(data.frame(x = 1:5, y = 1:5)) + geom_point(aes(x, y))
#' p + theme_map_minimal()
theme_map_minimal <- function(base_size = 12, base_family = "calb") {
  ggplot2::`%+replace%`(theme_mvl(base_size = base_size, base_family = base_family),
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      line = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
      legend.title = ggplot2::element_text()
    )
  )
}

#' McDonald's ggplot theme.
#'
#'A common theme for McDonalds plots.
#'
#' @param base_size The base size for fonts
#' @param base_family The base family for fonts
#'
#' @return A theme object to be added to ggplot objects
#' @export theme_mcd
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(data.frame(x = 1:5, y = 1:5)) + geom_point(aes(x, y))
#' p + theme_mcd()
theme_mcd <- function(base_size = 12, base_family = "") {
  ggplot2::`%+replace%`(ggplot2::theme_bw(base_size = base_size, base_family = base_family),
    ggplot2::theme(
      legend.key = ggplot2::element_rect(fill = '#F2F2F2'),
      legend.background = ggplot2::element_rect(fill = '#F2F2F2'),

      panel.background = ggplot2::element_rect(fill = '#F2F2F2'),
      panel.grid.major = ggplot2::element_line(colour = "white", size = 1),

      plot.background = ggplot2::element_rect(fill = '#F2F2F2', colour = '#F2F2F2'),
      plot.title = ggplot2::element_text(face = "bold")
    )
  )
}



#' Add a bounding box to a map plot.
#'
#'Add a bounding box to a ggplot map and set the limits of the plot to the box.
#' @param long_limits The longitude range that the map should take
#' @param lat_limits The latitude range that the map should take
#' @param draw_border Should the border of the box be drawn?
#'
#' @return Adds a box to the outline of the plot sets the plot limits
#' @export bounding_box
bounding_box <- function(long_limits, lat_limits, draw_border = TRUE) {

  if(length(long_limits) != 2) {
    stop("The format for long_limits should be c(minimum, maximum)")
  }

  if(length(lat_limits) != 2) {
    stop("The format for lat_limits should be c(minimum, maximum)")
  }

  if(long_limits[2] < long_limits[1]) {
    stop("The format for long_limits should be c(minimum, maximum)")
  }

  if(lat_limits[2] < lat_limits[1]) {
    stop("The format for lat_limits should be c(minimum, maximum)")
  }

  # draw the bounding box
  if(draw_border) {
    list(
    ggplot2::geom_rect(xmin = long_limits[1], xmax = long_limits[2], ymin = lat_limits[1], ymax = lat_limits[2],
                       fill = "transparent",
                       colour = "black"),
    # fix the plot limits
    ggplot2::coord_cartesian(xlim = long_limits, ylim = lat_limits),
    # make sure that the axes do not pad the limits past the bounding box
    ggplot2::scale_y_continuous(expand = c(0, 0)),
    ggplot2::scale_x_continuous(expand = c(0, 0))
    )
    } else {
      list(
        # fix the plot limits
        ggplot2::coord_cartesian(xlim = long_limits, ylim = lat_limits),
        # make sure that the axes do not pad the limits past the bounding box
        ggplot2::scale_y_continuous(expand = c(0, 0)),
        ggplot2::scale_x_continuous(expand = c(0, 0))
      )
    }

}


#' Add a Marketview footnote to a plot.
#'
#' Adds a footer to the current plot with a datestamp and the Marketview
#' logo.
#'
#' @param footnote_text The text for the footnote to display
#' @param size The size of the footnote text
#' @param color The colour of the footnote text
#' @param mvl_png The location of the Marketview logo
#'
#' @return Adds a footnote to the current plot
#' @export make_footnote
make_footnote <- function(footnote_text = paste(format(Sys.time(), "%d %b %Y")),
                         size = 0.7,
                         color = "grey",
                         mvl_png = "M:/mvl_office/logo/2012/Marketview_Logo_Horizontal.png") {


  grid::pushViewport(grid::viewport())

  grid::grid.text(label= footnote_text ,
            x = grid::unit(1,"npc") - grid::unit(2, "mm"),
            y = grid::unit(2, "mm"),
            just = c("right", "bottom"),
            gp = grid::gpar(cex = size, col = color, fontface = "italic"))

  grid::grid.raster(png::readPNG(mvl_png),
              x = grid::unit(0,"npc") - grid::unit(2, "mm"),
              y = grid::unit(0,"npc") - grid::unit(5, "mm"),
              height = grid::unit(2, "cm"),
              just = c("left", "bottom"))

  grid::popViewport()
}



