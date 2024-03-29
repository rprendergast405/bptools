# plotting_functions.R
# -------------------------------------------------------------------------
# A script containing some functions commonly used when producing figures
# for reporting
# -------------------------------------------------------------------------
# Created by Bert on 05-07-2016
# -------------------------------------------------------------------------
# Last edited by Bert on 07-07-2016
# -------------------------------------------------------------------------

# Add calibri fonts ----
grDevices::windowsFonts(calb = "Calibri Bold",
                        cal = "Calibri",
                        hn = "Helvetica Neue",
                        hnb = "Helvetica Neue Bold")

#' ggplot theme.
#'
#'A minimal theme for plots that looks okay in a ppt report.
#'
#' @param base_size The base size for fonts
#' @param base_family The base family for fonts
#' @param plain_family The plain family for fonts (used in legend text)
#'
#' @return A theme object to be added to ggplot objects
#' @export theme_bp
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(data.frame(x = 1:5, y = 1:5)) + geom_point(aes(x, y))
#' p + theme_bp()
theme_bp <- function(base_size = 13, base_family = "hnb", plain_family = "hn", text_colour = "black") {



  ggplot2::`%+replace%`(ggplot2::theme_minimal(base_size = base_size, base_family = plain_family),
                        ggplot2::theme(
                          text = ggplot2::element_text(family = plain_family,
                                                       face = "plain",
                                                       colour = text_colour,
                                                       size = base_size,
                                                       hjust = 0.5,
                                                       vjust = 0.5,
                                                       angle = 0,
                                                       lineheight = 0.9,
                                                       margin = grid::unit(rep(0.1, 4), "cm"),
                                                       debug = FALSE),


                          legend.text = ggplot2::element_text(size = round(base_size * 0.9), face = "plain", family = plain_family),
                          legend.title = ggplot2::element_blank(),
                          legend.position = "top",
                          legend.direction = "horizontal",
                          legend.box = "vertical",

                          panel.grid.minor.x = ggplot2::element_blank(),
                          panel.grid.minor.y = ggplot2::element_blank(),
                          panel.spacing = unit(1, "cm"),
                          strip.text = ggplot2::element_text(family = base_family),

                          plot.title = ggplot2::element_text(size = round(base_size * 1.5), hjust = 0),
                          plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "cm"),

                          plot.subtitle = ggplot2::element_text(family = "geo", hjust = 0),
                          plot.caption = ggplot2::element_text(size = round(base_size * 0.85), family = plain_family, hjust = 1)
                        )
  )
}

#' Old Marketview ggplot theme.
#'
#'An old version of a minimal theme for plots that looks okay in a Marketview ppt report.
#'
#' @param base_size The base size for fonts
#' @param base_family The base family for fonts
#' @param plain_family The plain family for fonts (used in legend text)
#'
#' @return A theme object to be added to ggplot objects
#' @export theme_mvl_old
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(data.frame(x = 1:5, y = 1:5)) + geom_point(aes(x, y))
#' p + theme_mvl_old()
theme_mvl_old <- function(base_size = 12, base_family = "calb", plain_family = "cal", text_colour = "black") {
  if(!all(c("cal", "calb") %in% names(grDevices::windowsFonts()))) {
    grDevices::windowsFonts(calb = "Calibri Bold",
                            cal = "Calibri")

  }


  ggplot2::`%+replace%`(ggplot2::theme_minimal(base_size = base_size, base_family = base_family),
                        ggplot2::theme(
                          text = ggplot2::element_text(family = base_family,
                                                       face = "plain",
                                                       colour = text_colour,
                                                       size = base_size,
                                                       hjust = 0.5,
                                                       vjust = 0.5,
                                                       angle = 0,
                                                       lineheight = 0.9,
                                                       margin = grid::unit(rep(0.1, 4), "cm"),
                                                       debug = FALSE),

                          axis.title.x = ggplot2::element_blank(),
                          axis.title.y = ggplot2::element_blank(),

                          legend.text = ggplot2::element_text(size = base_size, face = "plain", family = plain_family),
                          legend.title = ggplot2::element_blank(),
                          legend.position = "top",
                          legend.direction = "horizontal",

                          panel.grid.major.x = ggplot2::element_blank(),

                          plot.title = ggplot2::element_text(size = base_size * 2, hjust = 0),
                          plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "cm")
                        )
  )
}

#' ggplot map theme (old version).
#'
#'A theme for maps that looks okay in a ppt report.
#'Similar to \code{\link{theme_mvl}} but it has a blue background, no axis labels,
#'and a floating legend.
#'
#' @param base_size The base size for fonts
#' @param base_family The base family for fonts
#' @param plain_family The plain family for fonts (used in legend text)
#'
#' @return A theme object to be added to ggplot objects
#' @export theme_map_old
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(data.frame(x = 1:5, y = 1:5)) + geom_point(aes(x, y))
#' p + theme_map()
theme_map_old <- function(base_size = 11, base_family = "hnb", plain_family = "hn", text_colour = "black") {
  if(!all(c("hn", "hnb") %in% names(grDevices::windowsFonts()))) {
    grDevices::windowsFonts(hn = "Helvetica Neue",
                            hnb = "Helvetica Neue Bold")

  }

  ggplot2::`%+replace%`(ggplot2::theme_minimal(base_size = base_size, base_family = base_family),
    ggplot2::theme(
      line = ggplot2::element_blank(),
      text = ggplot2::element_text(family = base_family,
                          face = "plain",
                          colour = text_colour,
                          size = base_size,
                          hjust = 0.5,
                          vjust = 0.5,
                          angle = 0,
                          lineheight = 0.9,
                          margin = grid::unit(rep(0.1, 4), "cm"),
                          debug = FALSE),

      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),

      legend.background = ggplot2::element_rect(fill = grDevices::rgb(1, 1, 1, 0.65), colour = "white"),
      legend.text = ggplot2::element_text(face = "plain", family = plain_family),
      legend.title = ggplot2::element_text(),
      legend.position = c(1, 1),
      legend.direction = "vertical",
      legend.justification = c(1, 1),
      legend.box = "vertical",

      strip.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = 'lightblue'),
      panel.grid.major.y = ggplot2::element_blank(),

      plot.title = ggplot2::element_text(size = ggplot2::rel(2), hjust = 0),
      plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    )
  )
}

#' ggplot map theme (minimal).
#'
#'A minimal theme for maps that is more suitable for faceted plots.
#'
#' @param base_size The base size for fonts
#' @param base_family The base family for fonts
#' @param plain_family The plain family for fonts (used in legend text)
#'
#' @return A theme object to be added to ggplot objects
#' @export theme_map
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(data.frame(x = 1:5, y = 1:5)) + geom_point(aes(x, y))
#' p + theme_map_minimal()
theme_map <- function(base_size = 11, base_family = "hnb", plain_family = "hn", text_colour = "black") {
  ggplot2::`%+replace%`(theme_mvl(base_size = base_size, base_family = base_family, text_colour = text_colour),
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      line = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = ggplot2::rel(2), hjust = 0),
      plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
      legend.text = ggplot2::element_text(face = "plain", family = plain_family),
      panel.grid = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank()
    )
  )
}


#' ggplot map theme (minimal).
#'
#'A minimal theme for maps that is more suitable for faceted plots.
#'
#' @param base_size The base size for fonts
#' @param base_family The base family for fonts
#' @param plain_family The plain family for fonts (used in legend text)
#'
#' @return A theme object to be added to ggplot objects
#' @export theme_map_minimal
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(data.frame(x = 1:5, y = 1:5)) + geom_point(aes(x, y))
#' p + theme_map_minimal()
theme_map_minimal <- theme_map

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
theme_mcd <- function (base_size = 11, base_family = "cenb", plain_family = "cen",
                       text_colour = 'black')
{
  if (!all(c("cen", "cenb") %in% names(grDevices::windowsFonts()))) {
    grDevices::windowsFonts(cen = "Century Gothic", cenb = "Century Gothic Bold")
  }
  ggplot2::`%+replace%`(ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family),
    ggplot2::theme(
      text = ggplot2::element_text(family = base_family, face = "plain", colour = text_colour, size = base_size,
                                   hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin = grid::unit(rep(0.1, 4), "cm"), debug = FALSE),
      legend.text = ggplot2::element_text(size = base_size, face = "plain", family = plain_family),
      legend.title = ggplot2::element_blank(),
      legend.position = "top", legend.direction = "horizontal",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = bptools::mvl_half_grey, size = 0.2),
      plot.title = ggplot2::element_text(size = round(base_size * 1.5), hjust = 0),
      plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
      plot.background = element_rect(fill = '#f2f2f2', colour = NA),
      panel.background = element_rect(fill = '#f2f2f2', colour = NA),
      strip.background = element_rect(fill = '#f2f2f2', colour = NA)))
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



#' Define a Zooming Function for Plotting Maps.
#'
#' For a shapefile in NZTM format return a 'zoom' function (a wrapper to coord_fixed()) which displays a square area centred on the shapefile.
#'
#' @param shp_dat A shapefile on which to base the plot window
#' @param x The name of the attribute in shp_dat which describes the horizontal coordinates
#' @param y The name of the attribute in shp_dat which describes the vertical coordinates
#' @param margin The margin that should be added to the plot area
#' @param expand If TRUE, adds a small expansion factor to the limits to ensure that data and axes don't overlap. If FALSE, limits are taken exactly from the data or xlim/ylim.
#' @param ratio The aspect ratio (x/y) that the resultant plot area should have
#'
#' @export zoom_definition
#'
#' @examples akl_zoom <- zoom_definition(akl_cau_13.df)
#' ggplot()+
#' geom_polygon(data = nz_tla_13.df,
#' aes(long, lat, group = group, fill = TLA == 76))+
#' akl_zoom()
zoom_definition <- function(shp_dat, x = 'long', y = 'lat', margin = 0, expand = FALSE, ratio = 1){

  if (sum(is.na(shp_dat[[x]])) + sum(is.na(shp_dat[[y]])) > 0) {
    warning("The data provided has NA coordinates. Check that your geographical standards are consistent")
    na_ind <- is.na(shp_dat[[x]]) | is.na(shp_dat[[y]])

    shp_dat <- shp_dat[!na_ind, ]
  }

  # caluculate the centre point of the shapefile
  centre_point <- c(mean(c(max(shp_dat[[x]]), min(shp_dat[[x]]))), mean(c(max(shp_dat[[y]]), min(shp_dat[[y]]))))

  #Calculate the longest dimension, x or y
  x_length <- max(shp_dat[[x]]) - min(shp_dat[[x]]) + margin * 2
  y_length <- max(shp_dat[[y]]) - min(shp_dat[[y]]) + margin * 2

  max_x <- max(x_length, x_length / ratio, y_length * ratio, y_length)
  min_x <- min(x_length, x_length / ratio, y_length * ratio, y_length)

  search_space <- seq(min_x - 100, max_x + 100, length.out = 100000)

  x_dim <- min(search_space[search_space >= x_length & (search_space / ratio) >= y_length])
  y_dim <- x_dim / ratio

  #Calculate the box coordinates
  max_coords <- centre_point + c(x_dim, y_dim) / 2
  min_coords <- centre_point - c(x_dim, y_dim) / 2

  #Function to apply the zoom
  ggplot2::coord_fixed(xlim = c(min_coords[1], max_coords[1]),
                       ylim = c(min_coords[2], max_coords[2]),
                       expand = expand)

}



#' Define a Zooming for Maps, Based on a Named Area.
#'
#' A wrapper function for zoom_definintion() which refers to places from mvldata::osm_places.df to define a bounding box for the map
#'
#' @param place_name Name of the City/Town/Suburb/etc to centre the map on
#' @param margin The margin that should be added to the plot area
#' @param expand If TRUE, adds a small expansion factor to the limits to ensure that data and axes don't overlap. If FALSE, limits are taken exactly from the data or xlim/ylim.
#' @param ratio The aspect ratio (x/y) that the resultant plot area should have
#'
#' @export zoom_place
#'
#' @examples
#' ggplot()+
#' geom_polygon(data = nz_tla_13.df,
#' aes(long, lat, group = group, fill = TLA == 76))+
#' zoom_place("Auckland")
zoom_place <- function(place_name, margin = 20000, expand = FALSE, ratio = 1) {

  if (!(place_name %in% unique(mvldata::osm_places.df$name))) {
    warning("This place doesn't exist in mvldata::osm_places.df. Returning a random location.
  Check sort(unique(mvldata::osm_places.df$name)) to see the available places.")

    place_name <- sample(unique(mvldata::osm_places.df$name), 1)

    warning(paste("Zoom fn based on", place_name))
  }

  if (ratio <= 0) {
    stop("You can't have a plot with a negative aspect ratio. 'ratio' must be greater than 0")
  }

  place_df <- mvldata::osm_places.df[mvldata::osm_places.df$name %in% place_name, ]

  zoom_fn <- zoom_definition(
    shp_dat = place_df,
    margin = margin,
    expand = expand,
    ratio = ratio
  )

  return(zoom_fn)
}


#' Add a Polygon Basemap
#'
#' Used to quickly add a basemap of NZ geographical areas to plots
#'
#' @param data Usually will be mvldata::nz_tla_06.df, nz_tla_13.df, or nz_regions.df; but can be any data.frame of polygons with [long, lat, group] attributes
#' @param fill colour to fill the polygons
#' @param colour colour of the polygon borders
#' @param size width of the border line
#'
#' @export add_basemap
add_basemap <- function(data = mvldata::nz_tla_06.df, fill = mvl_half_grey, colour = mvl_stone, size = 0.2, hole_fill = "white") {

  if ("hole" %in% names(data)) {
    obj <- list(ggplot2::geom_polygon(data = data, ggplot2::aes(x = long, y = lat, group = group),
                                      fill = fill, colour = colour, size = size, inherit.aes = FALSE),
                ggplot2::geom_polygon(data = dplyr::filter(data, hole), ggplot2::aes(long, lat, group = group),
                                      fill = hole_fill, colour = colour, size = size, inherit.aes = FALSE)
    )
  } else {

    obj <- ggplot2::geom_polygon(data = data, ggplot2::aes(x = long, y = lat, group = group),
                                 fill = fill, colour = colour, size = size, inherit.aes = FALSE)

  }

  return(obj)
}

#' Add Water Polygons to a Map
#'
#' Used to quickly add water polygons to plots
#'
#' @param rivers Should river polygons be added to the plot?
#' @param inland Should inland water polygons be added to the plot?
#' @param extra Should additional lakes/rivers not covered by inland and rivers be added?
#' @param fill colour to fill the polygons
#' @param colour colour of the polygon borders
#' @param size width of the border line
#' @param hole_fill what colour should holes be filled with?
#'
#' @export add_water
add_water <- function(rivers = TRUE, inland = TRUE, extra = TRUE,
                      fill = mvl_half_teal, colour = mvl_teal, size = 0.2,
                      hole_fill = mvl_half_grey) {

  if (!any(rivers, inland, extra)) {
    stop("You need to add at least one layer to the plot")
  }

  dat <- data.frame()

  if (rivers) {
    water_dat <- mvldata::rivers.df

    # spoof some polygon ids to make sure we have distinct groups
    #water_dat$id <- as.numeric(water_dat$id) + max_id
    water_dat$piece <- as.numeric(water_dat$piece)
    water_dat$group <- paste("rivers", water_dat$group, sep = ".")

    # make sure the dfs will bind
    dat <- dplyr::bind_rows(dat, water_dat)

  }

  if (inland) {
    #max_id <- max(as.numeric(dat$id)) + 1

    #if(is.infinite(max_id)) max_id <- 1

    water_dat <- mvldata::inland_water.df

    # spoof some polygon ids to make sure we have distinct groups
    #water_dat$id <- as.numeric(water_dat$id) + max_id
    water_dat$piece <- as.numeric(water_dat$piece)
    water_dat$group <- paste("inland", water_dat$group, sep = ".")

    # make sure the dfs will bind
    dat <- dplyr::bind_rows(dat, water_dat)
  }

  if (extra) {
   # max_id <- max(as.numeric(dat$id)) + 1

    #if(is.infinite(max_id)) max_id <- 1

    water_dat <- mvldata::extra_waters

    # spoof some polygon ids to make sure we have distinct groups
    #water_dat$id <- as.numeric(water_dat$id) + max_id
    water_dat$piece <- as.numeric(water_dat$piece)
    water_dat$group <- paste("extra", water_dat$group, sep = ".")

    dat <- dplyr::bind_rows(dat, water_dat)
  }


  list(ggplot2::geom_polygon(data = dat, ggplot2::aes(x = long, y = lat, group = group),
                             fill = fill, colour = colour, size = size, inherit.aes = FALSE),
       ggplot2::geom_polygon(data = dplyr::filter(dat, hole), ggplot2::aes(long, lat, group = group),
                             fill = hole_fill, colour = colour)
  )
}



#' Add Some Placenames to a Map
#'
#' Use to quickly add a layer of towns/cities and some selected suburbs to a map for reference
#'
#' @param data A data.frame of the names to add to the plot, and their locations in NZTM coordinates
#' @param colour The text colour for the place names
#' @param fillcolour Fill colour for the text. Only used if shadowtext package is installed.
#' @param size The text size for the place names
#' @param tla Optional argument to only show the suburbs of a given set of TLA ids (2006 TLA definition)
#'
#' @export add_placenames
add_placenames <- function(data = mvldata::sparse_places.df, colour = "black", bgcolour = "white", size = 3, tla = NULL) {


  if (!is.null(tla)) {

    stopifnot(is.integer(tla))

    data <- data[data[["TLA"]] %in% tla, ]
  }

  dat <- data[data[["osm_id"]] %in% mvldata::major_places.df[["osm_id"]] == FALSE, ]

  if ("shadowtext" %in% installed.packages()[, "Package"]) {
    obj <- list(shadowtext::geom_shadowtext(data = dat, ggplot2::aes(x = long, y = lat, label = name),
                                            color = colour, bg.color = bgcolour, size = size, inherit.aes = FALSE, family = "hn"),
                shadowtext::geom_shadowtext(data = mvldata::major_places.df, ggplot2::aes(x = long, y = lat, label = name),
                                            color = colour, bg.color = bgcolour, size = size, inherit.aes = FALSE, family = "hn")
    )
  } else {
    obj <- list(ggplot2::geom_text(data = dat, ggplot2::aes(x = long, y = lat, label = name),
                                   colour = colour, size = size, inherit.aes = FALSE, family = "hn"),
                ggplot2::geom_text(data = mvldata::major_places.df, ggplot2::aes(x = long, y = lat, label = name),
                                   colour = colour, size = size, inherit.aes = FALSE, family = "hnb")
    )

  }


  return(obj)
}

#' Add CAU Labels to a Map
#'
#' @param sp_dat A SpatialPolygonsDataFrame of the CAU boundaries
#' @param size text size
#' @param family text family
#' @param colour text colour
#' @param ... other arguments passed to \code{\link[ggplot2]{layer}}
#'
#' @export cau_names
cau_names <- function(sp_dat = mvldata::nz_cau_13.spdf, size = 1.5, family = "hn", colour = mvl_grey, ...) {
  catchment_labels <- as.data.frame(sp::coordinates(sp_dat))
  names(catchment_labels) <- c("long", "lat")

  catchment_labels <- bind_cols(catchment_labels, sp_dat@data)

  obj <- ggplot2::geom_text(ggplot2::aes(long, lat, label = CAU_NAME), data = catchment_labels,
                           size = size, family = family, colour = colour, inherit.aes = FALSE,
                           check_overlap = TRUE, ...)

  return(obj)

}
