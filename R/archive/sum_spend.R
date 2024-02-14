#' Add Spending across Given Attributes
#'
#' A convenience function for quickly adding the spending across attributes.
#'
#' @param dat A data frame to add the spending
#' @param ... Attributes to group by
#'
#' @export sum_spend
sum_spend <- function(dat, ..., var = SPEND, group = FALSE) {

  var <- dplyr::enquo(var)
  var_name = dplyr::quo_name(var)

  var_name <- grep(var_name, names(dat), ignore.case = TRUE, value = TRUE)
  var <- sym(var_name)

  atts <- dplyr::quos(...)

  dat <- dplyr::group_by(dat, !!!atts)
  dat <- dplyr::summarise(dat, !!var_name := sum(!!var, na.rm = TRUE))

  if (group == FALSE) dat <- dplyr::ungroup(dat)

  return(dat)
}
