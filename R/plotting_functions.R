# plotting_functions.R
# -------------------------------------------------------------------------
# A script containing some functions commonly used when producing figures
# for Marketview reporting
# -------------------------------------------------------------------------
# Created by Bert on 05-07-2016
# -------------------------------------------------------------------------
# Last edited by Bert on 05-07-2016
# -------------------------------------------------------------------------

# Add calibri fonts ----
grDevices::windowsFonts(calb = "Calibri Bold",
                        cal = "Calibri")

# theme_mvl
#   general Marketview theme
theme_mvl <- function(base_size = 16, base_family = "calb") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
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

      legend.text = ggplot2::element_text(size = base_size - 2, face = "plain", family = "cal"),
      legend.title = ggplot2::element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",

      panel.grid.major.x = ggplot2::element_blank(),

      plot.title = ggplot2::element_text(size = base_size*1.5, hjust = 0),
      plot.margin = grid::unit(c(0.1, 0, 1.2, 0.1), "cm")
    )
}

# theme_map
#   A theme for mapping
theme_map <- function(base_size = 12, base_family = "calb") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
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
}

# theme_map_minimal
#   A theme for more minimalist mapping, this is better for faceted maps
theme_map_minimal <- function(base_size = 12, base_family = "calb") {
  theme_mvl(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      line = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
      legend.title = ggplot2::element_text()
    )
}

# theme_mcd
#   A theme for plotting McDonalds results
theme_mcd <- function(base_size = 12, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      legend.key = ggplot2::element_rect(fill = '#F2F2F2'),
      legend.background = ggplot2::element_rect(fill = '#F2F2F2'),

      panel.background = ggplot2::element_rect(fill = '#F2F2F2'),
      panel.grid.major = ggplot2::element_line(colour = "white", size = 1),

      plot.background = ggplot2::element_rect(fill = '#F2F2F2', colour = '#F2F2F2'),
      plot.title = ggplot2::element_text(face = "bold")
    )
}


# bounding_box
#   Add a bounding box to a ggplot map and set the limits of the plot to the box
#
# Arguments
#   long_limits (num): The longitude range that the map should take
#   lat_limits (num): The latitude range that the map should take
bounding_box <- function(long_limits, lat_limits) {

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
  ggplot2::geom_rect(ggplot2::aes(xmin = long_limits[1], xmax = long_limits[2], ymin = lat_limits[1], ymax = lat_limits[2]),
                     fill = "transparent",
                     colour = "black") +
    # fix the plot limits
    ggplot2::coord_cartesian(xlim = long_limits, ylim = lat_limits) +
    # make sure that the axes do not pad the limits past the bounding box
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0))
}


# makeFootnote.R
# A function to add the Marketview logo and a date stamp to the footer of a ggplot object
make_footnote <- function(footnoteText = paste(format(Sys.time(), "%d %b %Y")),
                         size = 0.7,
                         color = "grey",
                         mvl_png = "M:/mvl_office/logo/2012/Marketview_Logo_Horizontal.png") {


  grid::pushViewport(grid::viewport())

  grid::grid.text(label= footnoteText ,
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



