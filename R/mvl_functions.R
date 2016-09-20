# mvl_functions.R
# -------------------------------------------------------------------------
# A script containing some misc day-to-day functions used in Marketview
# reporting
# -------------------------------------------------------------------------
# Created by Bert on 05-07-2016
# -------------------------------------------------------------------------
# Last edited by Bert on 12-07-2016
# -------------------------------------------------------------------------


#' Save a ggplot object.
#'
#' A function to save a plot using dimensions and resolution that are
#' suitable for a Marketview report.
#'
#' @param p A ggplot object
#' @param write_dir The directory in which the figure should be saved
#' @param name A character name that the saved object will have
#' @param fig_num A figure number that preceeds the name
#' @param shape The shape of the png ("square" for a figure in ppt with the text to the side, and "wide" for a figure in ppt with the text underneath)
#' @param res The resolution of the png
#' @param mvl_footer Should a Marketview footer image be included?
#' @param ... if 'shape'
#'
#' @return Saves the plot to file
#' @export save_plot

save_plot <- function(p,
                      write_dir,
                      name,
                      fig_num = 1,
                      shape = "square",
                      res = 360,
                      mvl_footer = FALSE,
                      ...) {
  if(shape == "square"){
    height = 700*4
    width = 850*4
  } else if(shape == "wide"){
    height = (11.07*35/0.77)*4
    width = (23.92*35/0.77)*4
  }
  figure_name <- file.path(write_dir, paste0(fig_num, "_", name, ".png"))
  png(filename = figure_name,
      height = height,
      width = width,
      res = res)

  if(mvl_footer){
    print(p +
            theme(plot.margin = unit(c(.1, .1, .5, .1), units = 'in')))
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
                    mvl_foot = FALSE,
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
            theme(plot.margin = unit(c(.1, .1, .5, .1), units = 'in')))
    make_footnote(footnote_text = mvl_foot_text, color = mvl_foot_colour, size = mvl_foot_size)
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
    write.xlsx(x, file = file.path(write_dir, xls_name), sheetName = name, append = tab_num > 1)

    if(tab_num == 1) warning("An existing .xlsx may have been overwritten.")
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
mdollar <- function(x, dp = 2, form = "f", ...){
  paste0("$", formatC(x/1e6, digits = dp, format = form, ...), "B")
}



#' Label in billions of dollars
#'
#' A modification of the \code{dollar()} function from \code{scales}, which
#' is more appropriate for plots of large dollar values
#' @param x A numeric dollar amount
#'
#' @return A character representation of x in billions of dollars
#'
#' @examples
#' bdollar(1000000000)
#' bdollar(c(2000000000, 15000000000))
#' @export bdollar
bdollar <- function(x, dp = 2, form = "f", ...){
  paste0("$", formatC(x/1e9, digits = dp, format = form, ...), "B")
}
