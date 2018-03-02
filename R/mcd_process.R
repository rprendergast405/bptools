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

  if (any(c("DAYPART", "SPEND_BKT", "DAYPART") %in% colnames(dat))) {
    warning("Your table attributes indicate that you've used mcd_bp_final_card_dataset. You should use mcd_bp_final_card_dataset2.")
  }

  # coerce to data.frame
  dat <- as.data.frame(dat)

  # SEQMONTH if it exists
  if ("SEQMONTH" %in% colnames(dat)) {
    if (is.integer(dat[, "SEQMONTH"])) {
      dat[,"m_date"] <- parse_seqmonth(dat[,"SEQMONTH"])
    }
  }


    #Format Daypart Factor
  if ("DAYPART" %in% colnames(dat)) {
    if (is.character(dat[, "DAYPART"])) {
      dat[,"DAYPART"] <- factor(dat[["DAYPART"]], levels = c("Breakfast", "Snack AM", "Lunch",
                                                             "Snack PM", "Dinner", "Snack Later", "Extended"))
      }
  }

  if ("DAYPART_NO" %in% colnames(dat)) {
    warning("Your data contains a `DAYPART_NO` attribute; you should use `DAYPART_REPORT_NO`")
    if (is.integer(dat[, "DAYPART_NO"])) {
      dat[,"DAYPART"] <- factor(dat[["DAYPART_NO"]], levels = 1:7,
                                labels = c("Breakfast", "Snack AM", "Lunch",
                                           "Snack PM", "Dinner", "Snack Later", "Extended"))
    }
  }

  if ("DAYPART_REPORT_NO" %in% colnames(dat)) {
    if (is.integer(dat[, "DAYPART_REPORT_NO"])) {
      dat[,"DAYPART"] <- factor(dat[["DAYPART_REPORT_NO"]], levels = 1:7,
                                labels = c("Breakfast", "Snack AM", "Lunch",
                                           "Snack PM", "Dinner", "Snack Later", "Extended"))
    }
  }


  # Format the basket size attribute
  if ("SPEND_BKT" %in% colnames(dat)) {
    if (is.character(dat[, "SPEND_BKT"])) {
      dat[, "SPEND_BKT"] <- reorder(factor(dat[["SPEND_BKT"]]),  as.numeric(gsub(x = gsub(x = dat[["SPEND_BKT"]], pattern = " -.*", replacement = ""),
                                                                                 pattern = "[^0-9\\.]", replacement = "")))
    }
  }

  if ("SPEND_BKT_NO" %in% colnames(dat)) {
    if (is.integer(dat[, "SPEND_BKT_NO"])) {
      dat[, "SPEND_BKT"] <- factor(dat[["SPEND_BKT_NO"]],
                                   levels = 1:11,
                                   labels = c("$0 - $4.99",
                                              "$5 - $9.99",
                                              "$10 - $14.99",
                                              "$15 - $19.99",
                                              "$20 - $24.99",
                                              "$25 - $29.99",
                                              "$30 - $34.99",
                                              "$35 - $39.99",
                                              "$40 - $49.99",
                                              "$50 - $99.99",
                                              "$100 or More"))
    }
  }

  #Format Age Factor
  if ("AGE" %in% colnames(dat)) {
    if (is.character(dat[, "AGE"])) {
      dat[,"AGE"] <- factor(dat[["AGE"]])
    }
  }

  if ("AGEX" %in% toupper(colnames(dat))) {
    if (is.character(dat[, "AGEX"])) {
      dat[, "AGEX"] <- factor(dat[["AGEX"]])
      if ("AGE" %in% toupper(colnames(dat))) {
        dat[,"AGE"] <- as.integer(dat[["AGE"]])
      }
    }
  }

  if ("AGE_NO" %in% colnames(dat)) {
    if (is.integer(dat[, "AGE_NO"])) {
      dat[,"AGE"] <- factor(dat[["AGE_NO"]],
                            levels = 1:6,
                            labels = c("15 - 17",
                                       "18 - 29",
                                       "30 - 44",
                                       "45 - 54",
                                       "55 - 64",
                                       "65+"))
    }
  }

  # Deprivation
  if ("DEPRIVATION_NO" %in% colnames(dat)) {
    if (is.integer(dat[, "DEPRIVATION_NO"])) {
      dat[,"DEPRIVATION_NO"] <- factor(dat[["DEPRIVATION_NO"]],
                            levels = 1:4,
                            labels = c("Low Deprivation",
                                       "Moderate Deprivation",
                                       "High Deprivation",
                                       "Unknown"))
    }
  }

  #Format Gender Factor
  if ("GENDER" %in% colnames(dat)) {
    if (is.character(dat[, "GENDER"])) {
      dat[,"GENDER"] <- factor(dat[["GENDER"]], levels = c("M", "F"), labels = c("Male", "Female"))
    }
  }

  #Format IEO group factor
  if ("IEO_SEGMENT" %in% colnames(dat)) {
    if (is.character(dat[, "IEO_SEGMENT"])) {
      dat[dat[,"IEO_SEGMENT"] == "McDonalds NZ", "IEO_SEGMENT"] <- "McDonald's"
      dat[dat[,"IEO_SEGMENT"] == "QSR Competitors - Chicken/ Pizza/ Sandwich", "IEO_SEGMENT"] <- "QSR - Chicken/Pizza/Sandwich"
      dat[dat[,"IEO_SEGMENT"] == "QSR Competitors - Burger", "IEO_SEGMENT"] <- "QSR - Burger"

      dat[,"IEO_SEGMENT"] <- factor(dat[["IEO_SEGMENT"]], levels = c("McDonald's", "QSR - Burger", "QSR - Chicken/Pizza/Sandwich",
                                                                     "Independent Takeaway", "Coffee Cafes Bakeries", "Restaurants"))

      dat[, "QSR"] <- grepl("McD|QSR", dat[["IEO_SEGMENT"]])
    }
  }

  if ("BUNCH_NAMEZ" %in% colnames(dat)) {
    if (is.character(dat[, "BUNCH_NAMEZ"])) {
      dat[dat[,"BUNCH_NAMEZ"] == "McDonalds NZ", "BUNCH_NAMEZ"] <- "McDonald's"
      dat[dat[,"BUNCH_NAMEZ"] == "QSR Chicken/ Pizza/ Sandwich", "BUNCH_NAMEZ"] <- "QSR - Chicken/Pizza/Sandwich"
      dat[dat[,"BUNCH_NAMEZ"] == "QSR Burger", "BUNCH_NAMEZ"] <- "QSR - Burger"

      dat[,"BUNCH_NAMEZ"] <- factor(dat[["BUNCH_NAMEZ"]], levels = c("McDonald's", "QSR - Burger", "QSR - Chicken/Pizza/Sandwich",
                                                                     "Independent Takeaway", "Coffee Cafes Bakeries", "Restaurants"))

      dat[, "QSR"] <- grepl("McD|QSR", dat[["BUNCH_NAMEZ"]])
    }
  }


  if ("BUNCH_NAMEX" %in% colnames(dat)) {
    if (is.character(dat[, "BUNCH_NAMEX"])) {
      dat[dat[,"BUNCH_NAMEX"] == "McDonalds NZ", "BUNCH_NAMEX"] <- "McDonald's"

      dat[,"BUNCH_NAMEX"] <- factor(dat[["BUNCH_NAMEX"]], levels = c("McDonald's", "QSR Competitors",
                                                                     "Independent Takeaway", "Coffee Cafes Bakeries", "Restaurants"))

      dat[, "QSR"] <- grepl("McD|QSR", dat[["BUNCH_NAMEX"]])
    }
  }

  #Format HVC
  if ("CUST_TYPE" %in% toupper(colnames(dat)) & !("SHVC_CUST_TYPE" %in% toupper(colnames(dat)))) {
    if (is.character(dat[, "CUST_TYPE"])) {
      if ("H" %in% unique(dat[, "CUST_TYPE"])) {
        dat[is.na(dat[, "CUST_TYPE"]), "CUST_TYPE"] <- ""
        dat[dat[, "CUST_TYPE"] == "H", "CUST_TYPE"] <- "HVC"
        dat[dat[, "CUST_TYPE"] == "", "CUST_TYPE"] <- "non-HVC"
      }
      dat[, "CUST_TYPE"] <- factor(dat[["CUST_TYPE"]],
                                   levels = c("HVC", "non-HVC"))
    }
  }

  if ("SHVC_CUST_TYPE" %in% toupper(colnames(dat)) & !("CUST_TYPE" %in% toupper(colnames(dat)))) {
    if (is.character(dat[, "SHVC_CUST_TYPE"])) {
      if ("H" %in% unique(dat[, "SHVC_CUST_TYPE"])) {
        dat[is.na(dat[, "SHVC_CUST_TYPE"]), "SHVC_CUST_TYPE"] <- ""
        dat[dat[, "SHVC_CUST_TYPE"] == "H", "SHVC_CUST_TYPE"] <- "SHVC"
        dat[dat[, "SHVC_CUST_TYPE"] == "", "SHVC_CUST_TYPE"] <- "non-SHVC"
      }
      dat[, "SHVC_CUST_TYPE"] <- factor(dat[["SHVC_CUST_TYPE"]],
                                        levels = c("SHVC", "non-SHVC"))
    }
  }

  if ("SHVC_CUST_TYPE" %in% toupper(colnames(dat)) & "CUST_TYPE" %in% toupper(colnames(dat))) {
    if (is.character(dat[, "SHVC_CUST_TYPE"])) {
      if ("H" %in% unique(dat[, "SHVC_CUST_TYPE"])) {
        dat[is.na(dat[, "SHVC_CUST_TYPE"]), "SHVC_CUST_TYPE"] <- ""
      }

      if (is.character(dat[, "CUST_TYPE"])) {
        if ("H" %in% unique(dat[, "CUST_TYPE"])) {
          dat[is.na(dat[, "CUST_TYPE"]), "CUST_TYPE"] <- ""

          dat[,'COMBINED_CUST_TYPE'] <- ""
          dat[dat[, "SHVC_CUST_TYPE"] == "H", "COMBINED_CUST_TYPE"] <- "SHVC"
          dat[dat[, "SHVC_CUST_TYPE"] == "" & dat[, "CUST_TYPE"] == "H", "COMBINED_CUST_TYPE"] <- "HVC"
          dat[dat[, "SHVC_CUST_TYPE"] == "" & dat[, "CUST_TYPE"] == "", "COMBINED_CUST_TYPE"] <- "non-HVC"

          dat[,'COMBINED_CUST_TYPE'] <- factor(dat[["COMBINED_CUST_TYPE"]],
                                               levels = c("SHVC", "HVC", "non-HVC"))

          dat[dat[, "CUST_TYPE"] == "H", "CUST_TYPE"] <- "HVC"
          dat[dat[, "CUST_TYPE"] == "", "CUST_TYPE"] <- "non-HVC"
          dat[, "CUST_TYPE"] <- factor(dat[["CUST_TYPE"]],
                                       levels = c("HVC", "non-HVC"))

          dat[dat[, "SHVC_CUST_TYPE"] == "H", "SHVC_CUST_TYPE"] <- "SHVC"
          dat[dat[, "SHVC_CUST_TYPE"] == "", "SHVC_CUST_TYPE"] <- "non-SHVC"
          dat[, "SHVC_CUST_TYPE"] <- factor(dat[["SHVC_CUST_TYPE"]],
                                            levels = c("SHVC", "non-SHVC"))


        }
      }
    }
  }



  #Format NZDep scale
  if ("DEPRIVATION" %in% colnames(dat)) {
    dat[,"DEPRIVATION"] <- factor(dat[["DEPRIVATION"]], labels = c("Low Deprivation",
                                                                   "Moderate Deprivation",
                                                                   "High Deprivation",
                                                                   "Unknown"))
  }

  #Create Date class attribute
  if ("SEQDAY" %in% colnames(dat)) {
    dat[,"DATE"] <- as.Date(as.character(dat[["SEQDAY"]]), format = "%Y%m%d")
  }

  # Format the MCD_REGION attribute
  if ("MCD_REGION" %in% colnames(dat)) {
    if (is.character(dat[, "MCD_REGION"])) {
      dat[, "MCD_REGION"] <- factor(dat[["MCD_REGION"]], levels = c("Auckland", "NIPS", "Wellington", "South Island"))
    }
  }

  # Add a mcd_region attribute if it doesn't already exist
  if ("MERCH_TLA" %in% colnames(dat) & !("MCD_REGION" %in% colnames(dat))) {
    dat[, "MCD_REGION"] <- ifelse(dat[, "MERCH_TLA"] %in% c(1:11, 76), "Auckland",
                                  ifelse(dat[, "MERCH_TLA"] %in% 12:39, "NIPS",
                                         ifelse(dat[, "MERCH_TLA"] %in% 40:50, "Wellington", "South Island")))
    dat[, "MCD_REGION"] <- factor(dat[["MCD_REGION"]], levels = c("Auckland", "NIPS", "Wellington", "South Island"))
  }

  # coerce to tbl
  dat <- dplyr::as.tbl(dat)


  return(dat)
}
