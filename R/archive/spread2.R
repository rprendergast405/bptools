#' Spread a key-value pair across multiple columns.
#'
#' This is a wrapper for \code{tidyr::spread}, which has an additional argument \code{...}, where you can list any other attributes other than \code{key} and \code{value} that should be kept in the output. All other attributes are dropped.
#'
#' @param data A data frame.
#' @param key Column names or positions. This is passed to tidyselect::vars_pull().
#' @param value These arguments are passed by expression and support quasiquotation (you can unquote column names or column positions).
#' @param ... Which additional columns in \code{data} should be kept in the output?
#' @param fill If set, missing values will be replaced with this value. Note that there are two types of missingness in the input: explicit missing values (i.e. NA), and implicit missings, rows that simply aren't present. Both types of missing value will be replaced by fill.
#' @param convert If TRUE, type.convert() with asis = TRUE will be run on each of the new columns. This is useful if the value column was a mix of variables that was coerced to a string. If the class of the value column was factor or date, note that will not be true of the new columns that are produced, which are coerced to character before type conversion.
#' @param drop If FALSE, will keep factor levels that don't appear in the data, filling in missing combinations with fill.
#' @param sep If NULL, the column names will be taken from the values of key variable. If non-NULL, the column names will be given by "<key_name><sep><key_value>".
#'
#' @export spread2
spread2 <- function(data, key, value, ..., fill = NA, convert = FALSE, drop = TRUE,
                    sep = NULL) {
  keyq <- enquo(key)
  valueq <- enquo(value)
  keep <- enquos(...)

  data <- dplyr::select(data, !!keyq, !!valueq, !!!keep)

  # data$rownum <- 1:nrow(data)

  out <- tidyr::spread(data = data, key = !!keyq, value = !!valueq, fill = fill, convert = convert, drop = drop, sep = sep)

  # out <- dplyr::select(out, -rownum)

  return(out)
}
