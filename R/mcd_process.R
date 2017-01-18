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


  if ("AGEX" %in% toupper(colnames(dat))) {
    dat[, "AGEX"] <- factor(dat[["AGEX"]])
    if("AGE" %in% toupper(colnames(dat))) {
      dat[,"AGE"] <- as.integer(dat[["AGE"]])
    }
  }

  #Format Gender Factor
  if("GENDER" %in% colnames(dat)){
  dat[,"GENDER"] <- factor(dat[["GENDER"]], levels = c("M", "F"), labels = c("Male", "Female"))}

  #Format IEO group factor
  if("IEO_SEGMENT" %in% colnames(dat)){
  dat[dat[,"IEO_SEGMENT"] == "McDonalds NZ", "IEO_SEGMENT"] <- "McDonald's"
  dat[dat[,"IEO_SEGMENT"] == "QSR Competitors - Chicken/ Pizza/ Sandwich", "IEO_SEGMENT"] <- "QSR - Chicken/Pizza/Sandwich"
  dat[dat[,"IEO_SEGMENT"] == "QSR Competitors - Burger", "IEO_SEGMENT"] <- "QSR - Burger"

  dat[,"IEO_SEGMENT"] <- factor(dat[["IEO_SEGMENT"]], levels = c("McDonald's", "QSR - Burger", "QSR - Chicken/Pizza/Sandwich",
                                                        "Independent Takeaway", "Coffee Cafes Bakeries", "Restaurants"))

  dat[, "QSR"] <- dat[["IEO_SEGMENT"]] %in% c("McDonald's", "QSR Competitors - Chicken/ Pizza/ Sandwich", "QSR - Chicken/Pizza/Sandwich")
  }

  #Format HVC
  if("CUST_TYPE" %in% colnames(dat)){
    dat[is.na(dat[, "CUST_TYPE"]), "CUST_TYPE"] <- ""
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

  # Format the MCD_REGION attribute
  if("MCD_REGION" %in% colnames(dat)){
    dat[, "MCD_REGION"] <- factor(dat[["MCD_REGION"]], levels = c("Auckland", "NIPS", "Wellington", "South Island"))
  }

  # Add a mcd_region attribute if it doesn't already exist
  if("MERCH_TLA" %in% colnames(dat) & !("MCD_REGION" %in% colnames(dat))){
    dat[, "MCD_REGION"] <- ifelse(dat[, "MERCH_TLA"] %in% c(1:11, 76), "Auckland",
                                  ifelse(dat[, "MERCH_TLA"] %in% 12:39, "NIPS",
                                         ifelse(dat[, "MERCH_TLA"] %in% 40:50, "Wellington", "South Island")))
    dat[, "MCD_REGION"] <- factor(dat[["MCD_REGION"]], levels = c("Auckland", "NIPS", "Wellington", "South Island"))
  }

  # add a QSR indicator


  return(dat)
}
