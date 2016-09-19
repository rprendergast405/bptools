#' Fetch the location of a merchant from Fozzie.
#'
#' Given a group ID, \code{get_merchant_location()} returns a data frame containing the coordinates of the merchant, if it is available on Fozzie.
#'
#' @param db_con A connection to the BNZ database
#' @param group_id The group ID of the merchant of interest
#'
#' @export get_merchant_location
#'
#' @examples
#' DB <- odbcConnect("MVIEW", uid="user", pwd="password")
#' get_merchant_location(DB, group_id = 15435)
get_merchant_location <- function(db_con, group_id) {

  # I suppose I'd better try and prevent sql injections
  if(!is.numeric(group_id)) stop("group_id must be numeric")
  if(length(group_id) != 1) stop("you should only provide one group_id")

  db_qry <- paste0("
SELECT        X_COORD, Y_COORD
FROM          FOZZIE.GROUPED_RECEIVER gr
INNER JOIN    FOZZIE.GROUPED_RECEIVER_LOCATION grl ON gr.OID = grl.GROUP_ID


WHERE         OID = ", group_id)

  dat <- RODBC::sqlQuery(db_con, db_qry, stringsAsFactors = FALSE)

  return(dat)
}


#' Get the origin of BNZ spending for a given merchant.
#'
#' Given a group ID and a time frame, \code{bnz_spend_origin()} returns a data frame containing the spending by customer CAU.
#'
#' There might be some issues with the default date ranges if the current date is at the start or end of the month, but it
#' should be good enough and you can set it manually if you're concerned.
#'
#' @param db_con A connection to the BNZ database
#' @param group_id The group ID(s) of the merchant(s) of interest
#' @param seqmonth_start The first month from which to get spending. Defaults to 13 months prior to Sys.Date()
#' @param seqmonth_end The last month from which to get spending. Defaults to one month prior to Sys.Date()
#'
#' @export bnz_spend_origin
#'
#' @examples
#' DB <- odbcConnect("MVIEW", uid="user", pwd="password")
#' bnz_spend_origin(DB, group_id = 15435)
bnz_spend_origin <- function(db_con, group_id,
                             seqmonth_start = as.numeric(format(min(seq.Date(from = Sys.Date(), by = "-1 months", length.out = 13)), "%Y%m")),
                             seqmonth_end = as.numeric(format(seq.Date(from = Sys.Date(), by = "-1 months", length.out = 13)[2], "%Y%m"))) {

  # I suppose I'd better try and prevent sql injections
  if(!is.numeric(group_id)) stop("group_id must be numeric")
  if(!is.numeric(seqmonth_start)) stop("seqmonth_start must be numeric")
  if(length(seqmonth_start) != 1) stop("you should only provide one seqmonth_start")
  if(!is.numeric(seqmonth_end)) stop("seqmonth_end must be numeric")
  if(length(seqmonth_end) != 1) stop("you should only provide one seqmonth_end")


  db_qry <- paste0("
SELECT      c.CAU,
SUM(bnz.TRANSACTION_VALUE) AS SPEND

FROM        BNZTRANS.BNZTRANS bnz

LEFT JOIN   fozzie.grouped_elements gr
            ON bnz.RECEIVER_ID = gr.ELEMENT_ID

LEFT JOIN   fozzie.grouped_elements el
            ON bnz.receiver_id = el.element_id
            AND bnz.receiver_type = el.element_type

LEFT JOIN   fozzie.grouped_receiver gr2
            ON el.group_id = gr2.oid

INNER JOIN  bnztrans.customer c
            ON bnz.customer_id = c.customer_id


WHERE       bnz.SEQMONTH BETWEEN ", seqmonth_start, " AND ", seqmonth_end, "
            AND gr2.oid in (", paste(group_id, collapse = ", "), ")

GROUP BY    c.CAU"
  )

  dat <- RODBC::sqlQuery(db_con, db_qry, stringsAsFactors = FALSE)

  return(dat)
}
