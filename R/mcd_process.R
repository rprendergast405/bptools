#A wrapper function to automatically format data taken from mcd_bp_final_data_card_dataset.
#The attribute IEO_SEGMENT is defined as:
#case when a.bunch_id in (19150, 19151) then b.bunch_name else b.bunch_namex end as ieo_segment

#' Process Raw McDonalds Data.
#'
#' A wrapper function to automatically format data taken from mcd_bp_final_data_card_dataset.
#' The attribute IEO_SEGMENT is defined as:
#' case when a.bunch_id in (19150, 19151) then b.bunch_name else b.bunch_namex end as ieo_segment

#' @param dat A data.frame from the MCD_BP_FINAL_CARD_DATASET table
#'
#' @return A processed data frame for analysis
#' @export mcd_process
mcd_process <- function(dat){

    #Format Daypart Factor
  if("DAYPART" %in% colnames(dat)){
  dat[,"DAYPART"] <- factor(dat[["DAYPART"]], levels = c("Breakfast", "Snack AM", "Lunch",
                                                "Snack PM", "Dinner", "Snack Later", "Extended"))}

  # Format the basket size attribute
  if("SPEND_BKT" %in% colnames(dat)){
    dat[, "SPEND_BKT"] <- reorder(factor(dat[["SPEND_BKT"]]),  as.numeric(gsub(x = gsub(x = dat[["SPEND_BKT"]], pattern = " -.*", replacement = ""),
                                                        pattern = "[^0-9\\.]", replacement = "")))
  }

  #Format Age Factor
  if("AGE" %in% colnames(dat)){
  dat[,"AGE"] <- factor(dat[["AGE"]])}

  #Format Gender Factor
  if("GENDER" %in% colnames(dat)){
  dat[,"GENDER"] <- factor(dat[["GENDER"]], levels = c("M", "F"), labels = c("Male", "Female"))}

  #Format IEO group factor
  if("IEO_SEGMENT" %in% colnames(dat)){
  dat[dat[,"IEO_SEGMENT"] == "McDonalds NZ", "IEO_SEGMENT"] <- "McDonald's"
  dat[dat[,"IEO_SEGMENT"] == "QSR Competitors - Chicken/ Pizza/ Sandwich", "IEO_SEGMENT"] <- "QSR - Chicken/Pizza/Sandwich"
  dat[dat[,"IEO_SEGMENT"] == "QSR Competitors - Burger", "IEO_SEGMENT"] <- "QSR - Burger"

  dat[,"IEO_SEGMENT"] <- factor(dat[["IEO_SEGMENT"]], levels = c("McDonald's", "QSR - Burger", "QSR - Chicken/Pizza/Sandwich",
                                                        "Independent Takeaway", "Coffee Cafes Bakeries", "Restaurants"))}

  #Format HVC
  if("CUST_TYPE" %in% colnames(dat)){
    dat[dat[,"CUST_TYPE"] == "H", "CUST_TYPE"] <- "HVC"
    dat[dat[,"CUST_TYPE"] == "", "CUST_TYPE"] <- "non-HVC"

    dat[,"CUST_TYPE"] <- factor(dat[["CUST_TYPE"]], levels = c("HVC", "non-HVC"))}

  #Format NZDep scale
  if("DEPRIVATION" %in% colnames(dat)){
    dat[,"DEPRIVATION"] <- factor(dat[["DEPRIVATION"]])
  }

  #Create Date class attribute
  if("SEQDAY" %in% colnames(dat)){
    dat[,"DATE"] <- as.Date(as.character(dat[["SEQDAY"]]), format = "%Y%m%d")
  }

  return(dat)
}
