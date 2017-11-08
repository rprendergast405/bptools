#' Get the Weekly Sales for a TLD Item
#'
#' @param con ODBC connection to bespoke
#' @param codes The menu_item_no codes of the items
#' @param start_week The first promo week of the time period
#' @param end_week The last promo week of the time period
#' @param group_name Optional name attribute to add to the output data
#'
#' @export tld_sales_summary
tld_sales_summary <- function(con, codes, start_week, end_week, promo_week = TRUE, group_name = NULL) {

  # Create a table of the codes
  codes_tbl <- data.frame(MENU_ITEM_NO = codes)

  datetime <- format(Sys.time(), "%Y%m%d%H%M")
  tmp_tblname <- paste0("MCD_SALES", datetime)
  RODBC::sqlSave(channel = con, dat = codes_tbl, tablename = tmp_tblname, rownames = FALSE)

  # which table to use?
  if (promo_week) mcd_tbl <- "mcd.mcd_tld_item_summ_promo" else mcd_tbl <- "mcd.mcd_tld_item_summ_week"
  if (promo_week) wk_name <- "seqpromo_week" else wk_name <- "seqweek"

  # Build the query statement to get sales for all products during the time period
  qry <- glue::glue("SELECT * FROM {mcd_tbl} sum
                     INNER JOIN {tmp_tblname} codes ON sum.menu_item_no = codes.menu_item_no
                     AND {wk_name} BETWEEN {start_week} AND {end_week}")

  # Fetch the data
  summ_df <- RODBC::sqlQuery(con, qry)
  # Drop the codes table
  RODBC::sqlDrop(con, tmp_tblname)
  # Process the data
  summ_df <- dplyr::as.tbl(summ_df)
  summ_df <- dplyr::mutate_at(summ_df, dplyr::vars(dplyr::contains("WEEK")), as.integer)
  summ_df <- dplyr::mutate(summ_df, MENU_ITEM_NO = as.integer(MENU_ITEM_NO))


  if (dim(summ_df)[1] == 0) {
    warning("No data found in summary table")
    return(summ_df)
  }


  if (promo_week) {
    # Add date-class for promo weeks
    summ_df <- dplyr::left_join(summ_df, sqlQuery(con, "SELECT seqpromo_week, MIN(seqday) AS w_date FROM mcd.mcd_promo_week_dates GROUP BY seqpromo_week"),
                                by = "SEQPROMO_WEEK")
    summ_df <- dplyr::mutate(summ_df, w_date = lubridate::ymd(W_DATE))
    summ_df <- dplyr::select(summ_df, -W_DATE)

    # Calculate the AWUs for each item
    summ_df <- dplyr::group_by(summ_df, MENU_ITEM_NO)
    summ_df <- dplyr::mutate(summ_df, AWU = QUANTITY / max(SITES))
    summ_df <- dplyr::arrange(summ_df, SEQPROMO_WEEK)
    summ_df <- dplyr::ungroup(summ_df)
  } else {
    # Add date-class for promo weeks
    summ_df <- dplyr::left_join(summ_df, mvldata::seqweek.df, by = "SEQWEEK")
    summ_df <- dplyr::rename(summ_df, w_date = week_start)

    # Calculate the AWUs for each item
    summ_df <- dplyr::group_by(summ_df, MENU_ITEM_NO)
    summ_df <- dplyr::mutate(summ_df, AWU = QUANTITY / max(SITES))
    summ_df <- dplyr::arrange(summ_df, SEQWEEK)
    summ_df <- dplyr::ungroup(summ_df)
  }
  if (!is.null(group_name)) summ_df <- dplyr::mutate(summ_df, `group` = group_name)

  return(summ_df)
}
