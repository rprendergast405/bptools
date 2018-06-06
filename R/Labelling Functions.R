# mvl_functions.R
# -------------------------------------------------------------------------
# A script with useful functions for displaying numeric values in publications
# that humans will be reading
# -------------------------------------------------------------------------
# Created by Bert on 19-12-2017
# -------------------------------------------------------------------------
# Last edited by Bert on 12-07-2016
# -------------------------------------------------------------------------

# HELPFILE ----

#' Labelling functions
#'
#' @description
#' More comprehensive versions of \code{\link{scales}} functions for displaying numeric values in human-readable format.
#' These functions better display negative values and allow for improved control over decimal places in comparison to the versions from \code{scales}.
#'
#' @param x Numeric variable to format
#' @param dp The number of decimal points to display
#' @param fill If there are missing values, how should they be displayed?
#' @inheritParams base::formatC
#'
#' @section Available Functions:
#' The following functions are currently available for use:
#' \describe{
#'   \item{\code{dollar}}{For dollar values}
#'   \item{\code{percent}}{For decimal percentages}
#'   \item{\code{dollar_change}}{Changes in dollar values}
#'   \item{\code{percent_change}}{Changes in percentages}
#'   \item{\code{comma_change}}{Changes in numeric values}
#'   \item{\code{kdollar}}{Thousands of dollars}
#'   \item{\code{kcomma}}{Thousands}
#'   \item{\code{kdollar_change}}{Changes in thousands of dollars}
#'   \item{\code{kcomma_change}}{Changes in thousands}
#'   \item{\code{mdollar}}{Millions of dollars}
#'   \item{\code{mcomma}}{Millions}
#'   \item{\code{mdollar_change}}{Changes in mdollars}
#'   \item{\code{mcomma_change}}{Changes in millions}
#'   \item{\code{bdollar}}{Billions of dollars}
#'   \item{\code{bcomma}}{Billions}
#'   \item{\code{bdollar_change}}{Changes in bdollars}
#'   \item{\code{bcomma_change}}{Changes in billions}
#' }
#' If there are others that you want to be implemented, let me (Bert) know
#'
#' @name mvl_labellers
NULL


# BASE FUNCTIONS ----

#' @examples
#' dollar(1000000)
#' dollar(c(-2000000, 15000000))
#' @rdname mvl_labellers
#' @export dollar
dollar <- function(x, dp = 2, fill = "", form = "f", ...){

  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), "$", formatC(abs(x), digits = dp, format = form, big.mark = ",", ...))

  out[na_x] <- fill

  return(out)
}

#' @examples
#' comma(100000)
#' comma(c(-20000, 15000000))
#' @rdname mvl_labellers
#' @export comma
comma <- function(x, dp = 0, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), formatC(abs(x), digits = dp, format = form, big.mark = ",", ...))

  out[na_x] <- fill

  return(out)
}

#' @examples
#' percent(c(-0.20, 0.53))
#' @rdname mvl_labellers
#' @export percent
percent <- function(x, dp = 1, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(formatC(x * 100, digits = dp, format = form, big.mark = ",", ...), "%")

  out[na_x] <- fill

  return(out)
}

#' @examples
#' dollar_change(c(-200, 15))
#' @rdname mvl_labellers
#' @export dollar_change
dollar_change <- function(x, dp = 2, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), ifelse(x > 0, "+", ""), "$", formatC(abs(x), digits = dp, format = form, big.mark = ",", ...))

  out[na_x] <- fill

  return(out)
}

#' @examples
#' percent_change(c(-0.20, 0.53))
#' @rdname mvl_labellers
#' @export percent_change
percent_change <- function(x, dp = 1, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x > 0, "+", ""), formatC(x * 100, digits = dp, format = form, big.mark = ",", ...), "%")

  out[na_x] <- fill

  return(out)
}


#' @examples
#' comma_change(c(-2000000, 15000000))
#' @rdname mvl_labellers
#' @export comma_change
comma_change <- function(x, dp = 0, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), ifelse(x > 0, "+", ""), formatC(abs(x), digits = dp, format = form, big.mark = ",", ...))

  out[na_x] <- fill

  return(out)
}


# THOUSANDS ----

#' @examples
#' kdollar(1000000)
#' kdollar(c(-2000000, 15000000))
#' @rdname mvl_labellers
#' @export kdollar
kdollar <- function(x, dp = 1, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), "$", formatC(abs(x)/1e3, digits = dp, format = form, big.mark = ",", ...), "k")

  out[na_x] <- fill

  return(out)
}

#' @examples
#' kdollar(100000)
#' kdollar(c(-20000, 15000000))
#' @rdname mvl_labellers
#' @export kcomma
kcomma <- function(x, dp = 0, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), formatC(abs(x)/1e3, digits = dp, format = form, big.mark = ",", ...), "k")

  out[na_x] <- fill

  return(out)
}

#' @examples
#' kdollar_change(c(-200000, 15000))
#' @rdname mvl_labellers
#' @export kdollar_change
kdollar_change <- function(x, dp = 0, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), ifelse(x > 0, "+", ""), "$",
         formatC(abs(x) / 1e3, digits = dp, format = form, big.mark = ",", ...), "k")

  out[na_x] <- fill

  return(out)
}

#' @examples
#' kcomma_change(c(-2000000, 15000000))
#' @rdname mvl_labellers
#' @export kcomma_change
kcomma_change <- function(x, dp = 0, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), ifelse(x > 0, "+", ""),
         formatC(abs(x) / 1e3, digits = dp, format = form, big.mark = ",", ...), "k")

  out[na_x] <- fill

  return(out)
}


# MILLIONS ----

#' @examples
#' mdollar(1000000)
#' mdollar(c(-2000000, 15000000))
#' @rdname mvl_labellers
#' @export mdollar
mdollar <- function(x, dp = 1, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), "$", formatC(abs(x)/1e6, digits = dp, format = form, big.mark = ",", ...), "M")

  out[na_x] <- fill

  return(out)
}

#' @examples
#' mdollar(100000)
#' mdollar(c(-20000, 15000000))
#' @rdname mvl_labellers
#' @export mcomma
mcomma <- function(x, dp = 0, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), formatC(abs(x)/1e6, digits = dp, format = form, big.mark = ",", ...), "M")

  out[na_x] <- fill

  return(out)
}

#' @examples
#' mdollar_change(c(-2000000, 150000))
#' @rdname mvl_labellers
#' @export mdollar_change
mdollar_change <- function(x, dp = 1, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), ifelse(x > 0, "+", ""), "$",
         formatC(abs(x) / 1e6, digits = dp, format = form, big.mark = ",", ...), "M")

  out[na_x] <- fill

  return(out)
}

#' @examples
#' mcomma_change(c(-2000000, 15000000))
#' @rdname mvl_labellers
#' @export mcomma_change
mcomma_change <- function(x, dp = 0, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), ifelse(x > 0, "+", ""),
         formatC(abs(x) / 1e6, digits = dp, format = form, big.mark = ",", ...), "M")

  out[na_x] <- fill

  return(out)
}


# BILLIONS ----

#' @examples
#' bdollar(1000000)
#' bdollar(c(-2000000, 15000000))
#' @rdname mvl_labellers
#' @export bdollar
bdollar <- function(x, dp = 1, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), "$", formatC(abs(x)/1e9, digits = dp, format = form, big.mark = ",", ...), "B")

  out[na_x] <- fill

  return(out)
}

#' @examples
#' bdollar(100000)
#' bdollar(c(-20000, 15000000))
#' @rdname mvl_labellers
#' @export bcomma
bcomma <- function(x, dp = 0, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), formatC(abs(x)/1e9, digits = dp, format = form, big.mark = ",", ...), "B")

  out[na_x] <- fill

  return(out)
}

#' @examples
#' bdollar_change(c(-2000000, 150000))
#' @rdname mvl_labellers
#' @export bdollar_change
bdollar_change <- function(x, dp = 1, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), ifelse(x > 0, "+", ""), "$",
         formatC(abs(x) / 1e9, digits = dp, format = form, big.mark = ",", ...), "B")

  out[na_x] <- fill

  return(out)
}

#' @examples
#' bcomma_change(c(-2000000, 15000000))
#' @rdname mvl_labellers
#' @export bcomma_change
bcomma_change <- function(x, dp = 0, fill = "", form = "f", ...){
  na_x <- is.na(x)

  out <- paste0(ifelse(x < 0, "-", ""), ifelse(x > 0, "+", ""),
         formatC(abs(x) / 1e9, digits = dp, format = form, big.mark = ",", ...), "B")

  out[na_x] <- fill

  return(out)
}
