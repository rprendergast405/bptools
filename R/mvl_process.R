#' Process Raw Marketview Data.
#'
#' A function to format the standard analytics attributes which are stored as numeric variables to save table space

#' @param dat A data.frame from bespoke
#'
#' @return A processed data frame for analysis
#' @export mvl_process
mvl_process <- function(dat){

  # Time of day ----
  if("TOD" %in% colnames(dat)) {

    if(is.numeric(dat[, "TOD"])) {
      dat <- dplyr::left_join(dat, data.frame(TOD = 1:9,
                                              TOD2 = factor(c("Credit", "12:00am - 2:59am", "3:00am - 5:59am", "6:00am - 8:59am", "9:00am - 11:59am",
                                                              "12:00pm - 2:59pm", "3:00pm - 5:59pm", "6:00pm - 8:59pm", "9:00pm - 11:59pm"),
                                                            levels = c("12:00am - 2:59am", "3:00am - 5:59am", "6:00am - 8:59am", "9:00am - 11:59am",
                                                                       "12:00pm - 2:59pm", "3:00pm - 5:59pm", "6:00pm - 8:59pm", "9:00pm - 11:59pm", "Credit"))))
      dat$TOD <- dat$TOD2
      dat <- dat[, -which(names(dat) == "TOD2")]
    }
  }

  # Day of Week ---
  if("DOW" %in% colnames(dat)) {
    if(is.numeric(dat[, "DOW"])) {
      dat <- dplyr::left_join(dat, data.frame(DOW = 1:7,
                                              DOW2 = factor(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                                            levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))))
      dat$DOW <- dat$DOW2
      dat <- dat[, -which(names(dat) == "DOW2")]
    }
  }

  # Basket Size ----
  if("SPEND_BKT" %in% colnames(dat)) {
    if(is.numeric(dat[, "SPEND_BKT"])) {
      dat <- dplyr::left_join(dat, data.frame(SPEND_BKT = 1:14,
                                              SPEND_BKT2 = factor(c("$0.00 - $9.99",
                                                                    "$10.00 - $24.99",
                                                                    "$25.00 - $49.99",
                                                                    "$50.00 - $74.99",
                                                                    "$75.00 - $99.99",
                                                                    "$100.00 - $149.99",
                                                                    "$150.00 - $199.99",
                                                                    "$200.00 - $249.99",
                                                                    "$250.00 - $299.99",
                                                                    "$300.00 - $349.99",
                                                                    "$350.00 - $399.99",
                                                                    "$400.00 - $449.99",
                                                                    "$450.00 - $499.99",
                                                                    "$500+"),
                                                                  levels = c("$0.00 - $9.99",
                                                                             "$10.00 - $24.99",
                                                                             "$25.00 - $49.99",
                                                                             "$50.00 - $74.99",
                                                                             "$75.00 - $99.99",
                                                                             "$100.00 - $149.99",
                                                                             "$150.00 - $199.99",
                                                                             "$200.00 - $249.99",
                                                                             "$250.00 - $299.99",
                                                                             "$300.00 - $349.99",
                                                                             "$350.00 - $399.99",
                                                                             "$400.00 - $449.99",
                                                                             "$450.00 - $499.99",
                                                                             "$500+"))))
      dat$SPEND_BKT <- dat$SPEND_BKT2
      dat <- dat[, -which(names(dat) == "SPEND_BKT2")]
    }
  }
  return(dat)
}
