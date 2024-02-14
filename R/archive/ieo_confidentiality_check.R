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
