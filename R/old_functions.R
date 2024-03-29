# old_functions.R
# -------------------------------------------------------------------------
# These functions are not really in use any more, but they are an important
# piece in the history of the bptools package.
# -------------------------------------------------------------------------
# Created by Bert on 07-09-2017
# -------------------------------------------------------------------------




#' Save a ggplot object.
#'
#' A function to save a plot using dimensions and resolution that are
#' suitable for a ppt report.
#'
#' @param p A ggplot object
#' @param write_dir The directory in which the figure should be saved
#' @param name A character name that the saved object will have
#' @param fig_num A figure number that preceeds the name
#' @param shape The shape of the png ("square" for a figure in ppt with the text to the side, and "wide" for a figure in ppt with the text underneath)
#' @param res The resolution of the png
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
  if (shape == "square") {
    height = 700*4
    width = 850*4
  } else if (shape == "wide") {
    height = (11.07*35/0.77)*4
    width = (23.92*35/0.77)*4
  }
  figure_name <- file.path(write_dir, paste0(fig_num, "_", name, ".png"))
  png(filename = figure_name,
      height = height,
      width = width,
      res = res)

  print(p)

  dev.off()
}

#' Save of PNG for PowerPoint.
#'
#' Saves a ggplot object as a PNG that is suitable for a
#' report/presentation in ppt.
#'
#' @param p A ggplot object
#' @param filename The file_directory/name that the saved png will take
#' @param w The width of the saved png
#' @param h The height of the saved png
#' @param fam Font family to use
#' @param pm What margins should the saved plot have?
#' @param foot Should a footnote be included?
#' @param foot_text Which text should the footnote include?
#' @param foot_colour Which colour should the text be?
#' @param foot_size How large should the footnote be?
#'
#' @return Saves the plot to file
#' @export ppt_png
ppt_png <- function(p,
                    filename,
                    w = 22,
                    h = 12,
                    fam = "Calibri",
                    pm = c(.1, .1, .25, .1),
                    foot = FALSE,
                    foot_text = "",
                    foot_colour = mvl_grey,
                    foot_size = .8){

  png(filename,
      width = w,
      height = h,
      units = 'cm',
      res = 300,
      type = 'cairo',
      family = fam)

  if (foot) {
    print(p +
            theme(plot.margin = unit(c(.1, .1, .5, .1), units = 'in')))
    make_footnote(footnote_text = foot_text, color = foot_colour, size = foot_size)
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

  if (xlsx) {
    write.xlsx(x, file = file.path(write_dir, xls_name), sheetName = name, append = tab_num > 1)

    if (tab_num == 1) warning("An existing .xlsx may have been overwritten.")
  }
}


#A function to check the confidentiality standard against a group of IEO merchants
#Requires a DB connection, sql_string of merchants,
#a merchant first trans date and a merchant last trans date (defaults to last year)

ieo_confidentiality_check <- function(DB, groups,
                                      minday = format(Sys.time() - lubridate::years(1) - lubridate::weeks(1), '%Y%m%d'),
                                      maxday = format(Sys.time() - lubridate::weeks(1), '%Y%m%d')){

  vec_str <- vec_sql_string(unique(groups))

  test <- RODBC::sqlQuery(DB, paste("select count(distinct merch_id) merchant_count, bunch_namez, brand_id, brand_name
                                    from mcd_bp_grp_ids
                                    where merch_id in",
                                    groups,
                                    "and minday < ",
                                    minday,
                                    "and maxday >",
                                    maxday,
                                    'group by bunch_namez, brand_id, brand_name'))

  #Test that there are enough merchants in each group
  merch_test <- mcd_process(test)
  merch_test <- dplyr::group_by(merch_test, BUNCH_NAMEZ)
  merch_test <- dplyr::summarise(merch_test, MERCHANT_COUNT = sum(MERCHANT_COUNT))
  merch_test <- dplyr::mutate(merch_test,
                              confidentiality_standard = dplyr::case_when(MERCHANT_COUNT < 4 & BUNCH_NAMEZ != "McDonald's" ~ FALSE,
                                                                          TRUE ~ TRUE))
  merch_test <- merch_test[merch_test$confidentiality_standard, ]


  if (dim(merch_test)[1] > 0) {

    message('Less than four distinct merchants detected within IEO segments, you need to aggregate these segments:')
    print(merch_test)

  }


  brand_test <- mcd_process(test)
  brand_test <- dplyr::group_by(brand_test, BUNCH_NAMEZ)
  brand_test <- dplyr::summarise(brand_test, BRAND_COUNT = dplyr::n_distinct(BRAND_ID))
  brand_test <- dplyr::mutate(brand_test,
                              confidentiality_standard = dplyr::case_when(BRAND_COUNT < 4 & BUNCH_NAMEZ != "McDonald's" ~ FALSE,
                                                                          TRUE ~ TRUE))
  brand_test <- brand_test[brand_test$confidentiality_standard, ]

  if (dim(brand_test)[1] > 0) {

    message('Less than four distinct brands detected within IEO segments, you need to aggregate any QSR segments and may also need to do so for other segments:.')
    print(brand_test)

  }

}

