#' Add Spending across Given Attributes
#'
#' A convenience function for quickly adding the spending across attributes.
#'
#' @param dat A data frame to add the spending
#' @param ... Attributes to group by
#'
#' @export sum_spend
sum_spend <- function(dat, ...) {
  atts <- dplyr::quos(...)

  dat <- dplyr::group_by(dat, !!!atts)
  dat <- dplyr::summarise(dat, SPEND = sum(SPEND, na.rm = TRUE))
  dat <- dplyr::ungroup(dat)

  return(dat)
}
