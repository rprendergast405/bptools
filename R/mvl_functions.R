# mvl_functions.R
# -------------------------------------------------------------------------
# A script containing some misc day-to-day functions used in Marketview
# reporting
# -------------------------------------------------------------------------
# Created by Bert on 05-07-2016
# -------------------------------------------------------------------------
# Last edited by Bert on 05-07-2016
# -------------------------------------------------------------------------



# create_project
#   A function to create some common sub-directories used in an R project,
#   and import some scripts containing headers for common analysis tasks
#
# Arguments
#   - project_name (chr): The name that the project will take
#   - base_dir (chr): The base directory for the project
#   - within (chr): Optional path to a subdirectory within base_dir where the project will be located
#   - sub_dirs (chr): Optional names for any additional sub-direcories that the project should contain
#
# Returns
#   Logical, indicating that the project has been successfully created

create_project <- function(project_name,
                           base_dir = "M:/clients",
                           within,
                           sub_dirs = NULL) {

  # The default sub-directories that should be created by the function
  base_sub_dirs <- c("data", "data/processed", "src", "src/rmd", "src/functions", "src/shiny", "output", "output/tables", "output/figures", "doc")

  # The root directory for the project takes the project name
  if(!is.null(within)){
    root_dir <- file.path(base_dir, within, project_name)
  } else {
    root_dir <- file.path(base_dir, project_name)
  }

  # Add any additional user-specified sub-directories to the defaults
  sub_dirs <- c(base_sub_dirs, sub_dirs)

  # create the directories
  sapply(sub_dirs, FUN = function(x) {dir.create(file.path(root_dir, x), recursive = TRUE)})

  # create the .Rproj file with default specifications
  rproject_specs <- "Version: 1.0

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: pdfLaTeX"

  cat(paste(rproject_specs), file = file.path(root_dir, paste(project_name, "Rproj", sep = ".")))

  # import the script templates to the root directory

  file.copy(from = file.path("M:/R", "run_script.R"),
            to = root_dir,
            overwrite = FALSE,
            copy.mode = TRUE)

  file.copy(from = file.path("M:/R", "function_definition.R"),
            to = root_dir,
            overwrite = FALSE,
            copy.mode = TRUE)

  file.copy(from = file.path("M:/R", "0_initialise.R"),
            to = root_dir,
            overwrite = FALSE,
            copy.mode = TRUE)

  file.copy(from = file.path("M:/R", "1_processing.R"),
            to = root_dir,
            overwrite = FALSE,
            copy.mode = TRUE)

  file.copy(from = file.path("M:/R", "2_results.R"),
            to = root_dir,
            overwrite = FALSE,
            copy.mode = TRUE)
}


# map_get
#   A function to load a data frame containing the polygons for a given city
#
# Arguments
#   - city (chr): The city which the map will cover
#   - census_version (int): From which census definition should the polygons come from?
#   - map_dir (chr): Where are the RData files located?
map_get <- function(city = "Auckland", census_version = 2013, map_dir = "M:/R/map_data") {

  if(!(census_version %in% c(2006, 2013))) {
    stop("The census version should be either 2006 or 2013.")
  }

  map_name <- paste0(tolower(city), "_", version, ".RData")

  load(map_name)

  return(map.df)
}


#' Save a ggplot object
#'
#' A function to save a plot using dimensions and resolution that are
#' suitable for a Marketview report
#'
#' Arguments
#' @param p A ggplot object
#' @param write_dir The directory in which the figure should be saved
#' @param name A character name that the saved object will have
#' @param fig_num A figure number that preceeds the name
#' @param height The height of the png
#' @param width The width of the png
#' @param res The resolution of the png
#' @param mvl_footer Should a Marketview footer image be included?
save_plot <- function(p,
                      write_dir,
                      name,
                      fig_num = 1,
                      height = 700*4,
                      width = 850*4,
                      res = 360,
                      mvl_footer = FALSE) {
  figure_name <- file.path(write_dir, paste0(fig_num, "_", name, ".png"))
  png(filename = figure_name,
      height = height,
      width = width,
      res = res)

  if(mvl_foot){
    print(p +
            theme(plot.margin = unit(c(.1, .1, .25, .1), units = 'in')))
    make_footnote()
  } else  print(p)

  dev.off()
}

#' Save of PNG for PowerPoint
#'
#' Saves a ggplot object as a PNG that is suitable for a Marketview
#' report/presentation in ppt.
#'
#' @param p
#' @param filename
#' @param w
#' @param h
#' @param fam
#' @param pm
#' @param mvl_foot
#' @param mvl_foot_text
#' @param mvl_foot_colour
#' @param mvl_foot_size
#'
#' @return
#' @export
#'
#' @examples
ppt_png <- function(p,
                    filename,
                    w = 22,
                    h = 12,
                    fam = "Calibri",
                    pm = c(.1, .1, .25, .1),
                    mvl_foot = TRUE,
                    mvl_foot_text = "",
                    mvl_foot_colour = mvl_grey,
                    mvl_foot_size = .8){
  require(cairoDevice)

  png(filename,
      width = w,
      height = h,
      units = 'cm',
      res = 300,
      type = 'cairo',
      family = fam)

  if(mvl_foot){
    print(p +
            theme(plot.margin = unit(c(.1, .1, .25, .1), units = 'in')))
    make_footnote(footnoteText = mvl_foot_text, color = mvl_foot_colour, size = mvl_foot_size)
  } else print(p + theme(plot.margin = unit(pm, units = 'in')))

  dev.off()

}


#' Save data.frame to csv & xlsx
#'
#' @param x The table to save
#' @param write_dir The directory in which the tables should be saved
#' @param name The name of the saved object
#' @param tab_num A number that preceeds the name
#'
#' @return Saves the table to file
#'
#' @examples
save_table <- function(x,
                       write_dir,
                       name,
                       tab_num = 1,
                       xls_name = "all_tables.xlsx"){

    write_csv(x, path = file.path(write_dir, paste0(tab_num, "_", name, ".csv")))

    write.xlsx(x, file = file.path(write_dir, xls_name), sheetName = name, append = TRUE)
}


# mdollar
#   A function to label in millions of dollars
mdollar <- function(x) {
  paste0(scales::dollar(x/1000000), "M")
}
