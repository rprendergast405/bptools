#' Process Raw Australian IEO Data
#'
#' @param dat Raw data from the DB
#'
#' @return dat: original data frame with attributes cleaned/ordered and new groups added
#' @export mcda_process
mcda_process <- function(dat) {

  # Day of week
  if (any(grepl("wday", names(dat), ignore.case = TRUE))) {

    wday_names <- lubridate::wday(Sys.Date() + lubridate::days(1:7),
                                  label = TRUE, abbr = FALSE,
                                  week_start = 1)
    wday_names <- wday_names[order(wday_names)]
    wday_names <- as.character(wday_names)

    dat$wday_name <- factor(dat[[names(dat)[grepl("wday", names(dat), ignore.case = TRUE)]]],
                            labels = wday_names)
  }

  # IEO Categories
  if (any(grepl("IEO_CATEGORY", names(dat), ignore.case = FALSE))) {

    ieo_var <- names(dat)[grepl("IEO_CATEGORY", names(dat), ignore.case = TRUE)]

    # Tidy up any temporary groups
    dat[[ieo_var]][dat[[ieo_var]] == "TBD"] <- "FSR Varied Menu"
    dat[[ieo_var]][grepl("Caf", dat[[ieo_var]])] <- "Cafe"
    dat[[ieo_var]][dat[[ieo_var]] == "Missing"] <- "FSR Varied Menu"
    dat[[ieo_var]][dat[[ieo_var]] == "Coffee"] <- "Cafe"

    ieo_levels <- unique(dat[[ieo_var]])

    ieo_groups <- marketview::case_fwhen(ieo_levels == "McDonalds" ~ "McDonald's",
                                         grepl("QSR", ieo_levels) ~ "QSR Competitors",
                                         grepl("FSR", ieo_levels) ~ "FSR Competitors",
                                         TRUE ~ "Coffee, Cafes, Bakeries")

    ieo_segments <- marketview::case_fwhen(ieo_levels == "McDonalds" ~ "McDonald's",
                                           ieo_levels == "QSR Burger" ~ "QSR Burger",
                                           ieo_levels == "QSR Chicken" ~ "QSR Chicken",
                                           ieo_levels == "QSR Pizza" ~ "QSR Pizza",
                                           ieo_levels == "QSR Ethnic" ~ "QSR Ethnic",
                                           ieo_levels == "QSR Sandwich/ Salad/ Juice" ~ "QSR Sandwich/Salad/Juice",
                                           ieo_levels == "QSR Other" ~ "QSR Other",
                                           grepl("FSR", ieo_levels) ~ "FSR Competitors",
                                           TRUE ~ "Coffee, Cafes, Bakeries")


    ieo_order <- c("McDonalds", "QSR Burger", "QSR Chicken", "QSR Pizza", "QSR Ethnic",
                   "QSR Sandwich/Salad/Juice", "QSR Other", "FSR Asian",
                   "FSR Bar and Grill", "FSR Buffet/Cafeteria", "FSR Casino/Hotel",
                   "FSR Clubs", "FSR Entertainment", "FSR Italian", "FSR Other Ethnic",
                   "FSR Pubs/Hotels", "FSR Steak/Seafood", "FSR Varied Menu", "Bakery",
                   "Cafe")

    ieo_labels <- c("McDonald's", "QSR Burger", "QSR Chicken", "QSR Pizza", "QSR Ethnic",
                    "QSR Sandwich/Salad/Juice", "QSR Other", "FSR Asian",
                    "FSR Bar and Grill", "FSR Buffet/Cafeteria", "FSR Casino/Hotel",
                    "FSR Clubs", "FSR Entertainment", "FSR Italian", "FSR Other Ethnic",
                    "FSR Pubs/Hotels", "FSR Steak/Seafood", "FSR Varied Menu", "Bakeries",
                    "Cafes")

    ieo_tbl <- tibble::tibble(
      key = ieo_levels,
      ieo_group = ieo_groups,
      ieo_subgroup = factor(key, levels = ieo_order, labels = ieo_labels),
      ieo_segment = ieo_segments
    )
    key_var <- "key"
    dat <- dplyr::left_join(dat, ieo_tbl, by = setNames(key_var, ieo_var))
  }

  # Set the media markets using the postcodes
  # if (any(grepl("MERCHANT_POSTCODE", names(dat), ignore.case = TRUE)) & any(grepl("MERCHANT_STATE", names(dat), ignore.case = TRUE))) {
  #
  #   market_state <- sqlQuery(DB, "SELECT DISTINCT media_market as market_name, sih2sd AS mcd_state FROM mcda_market_state df
  #            INNER JOIN mcda_poa_2011_mcd_market mk ON df.market = mk.market_name") %>% setNames(tolower(names(.)))
  #
  #   postcode_market <- sqlQuery(DB, "SELECT * FROM mcda_postcode_tv_market") %>%
  #     setNames(tolower(names(.))) %>%
  #     dplyr::mutate(market_name = trim_ws(gsub("lsm|mkt", "", market_name, ignore.case = TRUE)),
  #                   state = tolower(state))
  #
  #
  #   market_mapping <- dplyr::left_join(postcode_market, market_state)
  #
  #   dat[["suburb_key"]] <- tolower(dat[[grep("MERCHANT_POSTCODE", names(dat), ignore.case = TRUE)]])
  #   dat[["state_key"]] <- tolower(dat[[grep("MERCHANT_STATE", names(dat), ignore.case = TRUE)]])
  #
  #   dat <- dat %>%
  #     left_join(market_mapping, by = c("suburb_key" = "suburb", "state_key" = "state")) %>%
  #     select(-contains("key")) %>%
  #     mutate(mcd_state = coalesce(mcd_state, MERCHANT_STATE),
  #            mcd_state = case_when(mcd_state == "ACT" ~ "NSW",
  #                                  mcd_state == "NT" ~ "QLD",
  #                                  TRUE ~ mcd_state))
  # }

  if (any(grepl("spend_bkt", names(dat), ignore.case = TRUE))) {

    bkt_var <- names(dat)[grepl("spend_bkt", names(dat), ignore.case = TRUE)]

    bkt_raw <- dat[[bkt_var]]

    dat[[bkt_var]] <- marketview::case_fwhen(bkt_raw == 1 ~ "$0 - $4.99",
                                             bkt_raw == 2 ~ "$5 - $10",
                                             bkt_raw == 3 ~ "$10 - $15",
                                             bkt_raw == 4 ~ "$15 - $20",
                                             bkt_raw == 5 ~ "$20 - $25",
                                             bkt_raw == 6 ~ "$25 - $30",
                                             bkt_raw == 7 ~ "$30 - $35",
                                             bkt_raw == 8 ~ "$35 - $40",
                                             bkt_raw == 9 ~ "$40 - $50",
                                             bkt_raw == 10 ~ "$50 - $75",
                                             bkt_raw == 11 ~ "$75 - $100",
                                             bkt_raw == 12 ~ "$100 - $200",
                                             TRUE ~ "$200+")

  }

  return(dat)
}
