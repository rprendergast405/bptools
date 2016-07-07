# mvl_functions.R
# -------------------------------------------------------------------------
# A script containing some misc day-to-day functions used in Marketview
# reporting
# -------------------------------------------------------------------------
# Created by Bert on 05-07-2016
# -------------------------------------------------------------------------
# Last edited by Bert on 07-07-2016
# -------------------------------------------------------------------------



#' Create a new R Project.
#'
#' Create some common sub-directories used in an R project, and import
#' some scripts containing headers for common analysis tasks.
#'
#' @param project_name The name that the project will take
#' @param base_dir The base directory for the project
#' @param within Optional path to a subdirectory within base_dir where the project will be located
#' @param sub_dirs Optional names for any additional sub-direcories that the project should contain
#'
#' @return Logical, indicating that the project has been successfully created
#' @export create_project
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


#' Save a ggplot object.
#'
#' A function to save a plot using dimensions and resolution that are
#' suitable for a Marketview report.
#'
#' @param p A ggplot object
#' @param write_dir The directory in which the figure should be saved
#' @param name A character name that the saved object will have
#' @param fig_num A figure number that preceeds the name
#' @param height The height of the png
#' @param width The width of the png
#' @param res The resolution of the png
#' @param mvl_footer Should a Marketview footer image be included?
#'
#' @return Saves the plot to file
#' @export save_plot

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

  if(mvl_footer){
    print(p +
            theme(plot.margin = unit(c(.1, .1, .25, .1), units = 'in')))
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
                    mvl_foot = TRUE,
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

  if(mvl_foot){
    print(p +
            theme(plot.margin = unit(c(.1, .1, .25, .1), units = 'in')))
    make_footnote(footnoteText = mvl_foot_text, color = mvl_foot_colour, size = mvl_foot_size)
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

  if(xlsx){
    write.xlsx(x, file = file.path(write_dir, xls_name), sheetName = name, append = TRUE)
  }
}


#' Label in millions of dollars
#'
#' A modification of the \code{dollar()} function from \code{scales}, which
#' is more appropriate for plots of large dollar values
#' @param x A numeric dollar amount
#'
#' @return A character representation of x in millions of dollars
#'
#' @examples
#' mdollar(1000000)
#' mdollar(c(2000000, 15000000))
#' @export mdollar
mdollar <- function(x) {
  paste0(scales::dollar(x/1000000), "M")
}
