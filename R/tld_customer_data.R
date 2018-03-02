
#' Get Matched TLD Customer Data
#'
#' @param con ODBC connection to bespoke
#' @param codes Set of codes for which to get the customers
#' @param start_week The first promo week of the time period
#' @param end_week The last promo week of the time period
#' @param keep_table Should the customer table be kept on the database? Use this if you don't want to re-run the data
#' @param tablename Name of the permanent table in the database, if you want to keep it
#'
#' @export tld_customer_data
tld_customer_data <- function(con, codes, start_week, end_week, keep_table = FALSE, tablename = NULL) {
  # Check inputs
  stopifnot(is.numeric(start_week), is.numeric(end_week), is.numeric(codes))

  if (keep_table) {
    if (grepl("[^a-zA-Z0-9_]", tablename)) stop("Please provide a tablename with only letter/number/underscore characters")

    if (nchar(tablename) > 30) stop("Your tablename is too long. Please give a tablename with 30 or fewer characters")

    if (toupper(tablename) %in% sqlTables(DB)$TABLE_NAME) stop(paste(toupper(tablename), "already exists. Please choose another tablename"))
  }

  # Initialise dates
  datetime <- paste0(substr(toupper(Sys.info()["user"]), 1, 3), format(Sys.time(), "%m%d%H%M"))

  date_qry <- glue::glue("SELECT MIN(seqday) AS period_start, MAX(seqday) AS period_end
                         FROM mcd.mcd_promo_week_dates WHERE seqpromo_week IN ({start_week}, {end_week})")
  week_df <- RODBC::sqlQuery(con, date_qry)

  start_day <- week_df$PERIOD_START
  end_day <- week_df$PERIOD_END

  start_month <- substr(start_day, 1, 6)
  end_month <- substr(end_day, 1, 6)

  # Create a table of the codes
  codes_tbl <- data.frame(MENU_ITEM_NO = codes)

  tmp_tblname <- paste0("MCD_SALES", datetime)
  RODBC::sqlSave(channel = con, dat = codes_tbl, tablename = tmp_tblname, rownames = FALSE)

  # Set up the queries
  # Get BNZ customer details
  cust_qry <- glue::glue("create table mcd_cust{datetime} compress nologging as
select c.customer_id,c.source_id,c.age,case when c.age between 15 and 17 then '15 - 17'
when c.age between 18 and 29 then '18 - 29'
when c.age between 30 and 44 then '30 - 44'
when c.age between 45 and 54 then '45 - 54'
when c.age between 55 and 64 then '55 - 64'
else '65+' end as agex,
c.gender,e.mb as meshblock
from bnztrans.customer c
inner join BNZTRANS.customer_ext_meshblocks d on c.customer_id = d.customer_id
inner join CENSUS.meshblock_info_2013 e on d.meshblock_13 = e.mb")

  # Get the HVC customer details
  hvc_qry <- glue::glue("create table mcd_cust{datetime}_2 compress nologging as
select source_id,age,max_month,cust_type from mcd_bp_cust_coding_final WHERE max_month BETWEEN {start_month} AND {end_month}")

  # Merge the two
  merge_qry <- glue::glue("create table mcd_cust{datetime}_3 compress nologging as
select a.source_id,b.customer_id,a.max_month,a.age,b.agex,b.meshblock,b.gender,a.cust_type
from mcd_cust{datetime}_2 a
INNER JOIN (select source_id,age,agex,gender,meshblock,max(customer_id) customer_id from mcd_cust{datetime} group by source_id,age,agex,gender,meshblock) b
on a.source_id = b.source_id and a.age = b.agex")


  # Index the table to speed up
  index_qry1 <- glue::glue("create index mcd_ind{datetime}1 on mcd_cust{datetime}_3 (customer_id)")
  index_qry2 <- glue::glue("create unique index mcd_ind{datetime}2 on mcd_cust{datetime}_3 (customer_id,max_month)")


  # Customer profile of those who buy the codes
  tld_cust_qry <- glue::glue("create table mcd_cust{datetime}_4 compress nologging as
SELECT DISTINCT source_id
FROM mcd.mcd_sales_items a
INNER JOIN mcd.trans_customer d on a.trans_id = d.trans_id
INNER JOIN mcd_cust{datetime}_3 c on d.customer_id = c.customer_id and SUBSTR(a.seqday, 1, 6) = c.max_month
INNER JOIN {tmp_tblname} codes ON a.menu_item_no = codes.menu_item_no
WHERE seqday BETWEEN {start_day} AND {end_day}")


  tld_final_qry <- glue::glue("create table mcd_cust{datetime}_5 compress nologging as
select a.trans_id, a.seqday, a.store_id, a.menu_item_no, a.register_no,
a.pos_type, mvcore.mcd_aggtime2(a.end_time) AS daypart_report_no, a.end_time,
a.total_amount, a.num_items AS total_items, a.quantity, a.item_price, a.item_type, a.base_plu,
c.source_id, c.agex, c.gender, c.meshblock, c.cust_type
from mcd.mcd_tld_data a
inner join MCD.TRANS_CUSTOMER d on a.trans_id = d.trans_id
inner join mcd_cust{datetime}_3 c on d.customer_id = c.customer_id and SUBSTR(a.seqday, 1, 6) = c.max_month
inner join mcd_cust{datetime}_4 c2 on c.source_id = c2.source_id

WHERE seqday BETWEEN {start_day} AND {end_day}")


  # Execute the queries ----
  cat("Building Customer Table (1 of 6)\n")
  RODBC::sqlQuery(con, cust_qry)
  cat("Getting HVC Data (2 of 6)\n")
  RODBC::sqlQuery(con, hvc_qry)
  cat("Merging (3 of 6)\n")
  RODBC::sqlQuery(con, merge_qry)
  cat("Indexing (4 of 6)\n")
  RODBC::sqlQuery(con, index_qry1)
  RODBC::sqlQuery(con, index_qry2)
  cat("Finding Customers (5 of 6)\n")
  RODBC::sqlQuery(con, tld_cust_qry)
  cat("Getting Final Data (6 of 6)\n")
  RODBC::sqlQuery(con, tld_final_qry)

  if (keep_table) {
    cat("Saving Final Data Table\n")
    keep_qry <- glue::glue("CREATE TABLE {toupper(tablename)} COMPRESS NOLOGGING AS
                            SELECT * FROM mcd_cust{datetime}_5")
    RODBC::sqlQuery(DB, keep_qry)
  }

  # get the final matched data
  tld_cust_data <- dplyr::as.tbl(sqlQuery(con, glue::glue("SELECT * FROM mcd_cust{datetime}_5")))

  # Drop the tables and return the data
  on.exit({
    cat("Dropping Tables\n")
    RODBC::sqlDrop(con, toupper(glue::glue("mcd_cust{datetime}")))
    RODBC::sqlDrop(con, toupper(glue::glue("mcd_cust{datetime}_2")))
    RODBC::sqlDrop(con, toupper(glue::glue("mcd_cust{datetime}_3")))
    RODBC::sqlDrop(con, toupper(glue::glue("mcd_cust{datetime}_4")))
    RODBC::sqlDrop(con, toupper(glue::glue("mcd_cust{datetime}_5")))
    RODBC::sqlDrop(con, toupper(glue::glue("{tmp_tblname}")))
  })



  return(tld_cust_data)
}

