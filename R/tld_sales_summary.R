#' Get the Weekly Sales for a TLD Item
#'
#' @param con ODBC connection to bespoke
#' @param codes The menu_item_no codes of the items
#' @param start_week The first promo week of the time period
#' @param end_week The last promo week of the time period
#' @param group_name Optional name attribute to add to the output data
#'
#' @export tld_sales_summary
tld_sales_summary <- function(con, codes, start_week, end_week, group_name = NULL) {

  # Create a table of the codes
  codes_tbl <- data.frame(MENU_ITEM_NO = codes)

  datetime <- format(Sys.time(), "%Y%m%d%H%M")
  tmp_tblname <- paste0("MCD_SALES", datetime)
  RODBC::sqlSave(channel = con, dat = codes_tbl, tablename = tmp_tblname, rownames = FALSE)

  # Build the query statement to get sales for all products during the time period
  qry <- glue::glue("SELECT * FROM mcd.mcd_tld_item_summ_promo sum
                     INNER JOIN {tmp_tblname} codes ON sum.menu_item_no = codes.menu_item_no
                     AND seqpromo_week BETWEEN {start_week} AND {end_week}")

  # Fetch the data
  summ_df <- RODBC::sqlQuery(con, qry)
  summ_df <- dplyr::as.tbl(summ_df)

  # Drop the codes table
  RODBC::sqlDrop(con, tmp_tblname)

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

  if (!is.null(group_name)) summ_df <- dplyr::mutate(summ_df, `group` = group_name)

  return(summ_df)
}
