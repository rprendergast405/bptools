#' Most Common Item Associations
#'
#' What are the best friends of a MCD tld item? Gets the number of items, total spending,
#' and total transactions for all items that have been bought in the same transaction
#' as the given items.
#'
#' @param con ODBC connection to bespoke database
#' @param codes The menu_item_no codes of the items to consider
#' @param start_week The first promo week of the time period to consider
#' @param end_week The last promo week of the time period to consider
#'
#' @export tld_item_friends
tld_item_friends <- function(con, codes, start_week, end_week) {
  # find the start and end days of the period
  date_qry <- glue::glue("SELECT MIN(seqday) AS period_start, MAX(seqday) AS period_end
                         FROM mcd.mcd_promo_week_dates WHERE seqpromo_week IN ({start_week}, {end_week})")
  week_df <- RODBC::sqlQuery(con, date_qry)

  start_day <- week_df$PERIOD_START
  end_day <- week_df$PERIOD_END

  # Build the query statement to get sales for all products during the time period
  qry <- glue::glue("
SELECT tld.menu_item_no, item_df.name, mvcore.mcd_aggtime2(start_time) AS daypart_report_no,
sum(case when tld.item_price = 0 then 0
when tld.item_type = 1 then abs(tld.quantity)
when tld.item_type = 5 then -abs(tld.quantity)
when tld.item_type = 49 then 0
end) as quantity,
sum(case when tld.item_type = 1 then abs(tld.quantity * tld.item_price)
when tld.item_type = 5 then -abs(tld.quantity * tld.item_price)
when tld.item_type = 49 then 0 end) as sales,
COUNT(DISTINCT trans.trans_id) AS transactions

FROM mcd.mcd_tld_data tld
INNER JOIN (SELECT DISTINCT trans_id FROM mcd.mcd_tld_data
WHERE seqday BETWEEN {start_day} AND {end_day} AND menu_item_no IN {vec_sql_string(codes)}) trans
ON trans.trans_id = tld.trans_id
LEFT JOIN mcd.mcd_menu_item item_df
ON item_df.code = tld.menu_item_no

WHERE seqday BETWEEN {start_day} AND {end_day}

GROUP BY item_df.name, tld.menu_item_no, mvcore.mcd_aggtime2(start_time)")

  # Fetch the data
  summ_df <- RODBC::sqlQuery(con, qry)
  summ_df <- dplyr::as.tbl(summ_df)

  return(summ_df)
}
