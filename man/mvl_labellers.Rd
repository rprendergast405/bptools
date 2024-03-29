% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Labelling Functions.R
\name{mvl_labellers}
\alias{mvl_labellers}
\alias{dollar}
\alias{comma}
\alias{percent}
\alias{dollar_change}
\alias{percent_change}
\alias{comma_change}
\alias{kdollar}
\alias{kcomma}
\alias{kdollar_change}
\alias{kcomma_change}
\alias{mdollar}
\alias{mcomma}
\alias{mdollar_change}
\alias{mcomma_change}
\alias{bdollar}
\alias{bcomma}
\alias{bdollar_change}
\alias{bcomma_change}
\title{Labelling functions}
\usage{
dollar(x, dp = 2, fill = "", form = "f", ...)

comma(x, dp = 0, fill = "", form = "f", ...)

percent(x, dp = 1, fill = "", form = "f", ...)

dollar_change(x, dp = 2, fill = "", form = "f", ...)

percent_change(x, dp = 1, fill = "", form = "f", ...)

comma_change(x, dp = 0, fill = "", form = "f", ...)

kdollar(x, dp = 1, fill = "", form = "f", ...)

kcomma(x, dp = 0, fill = "", form = "f", ...)

kdollar_change(x, dp = 0, fill = "", form = "f", ...)

kcomma_change(x, dp = 0, fill = "", form = "f", ...)

mdollar(x, dp = 1, fill = "", form = "f", ...)

mcomma(x, dp = 0, fill = "", form = "f", ...)

mdollar_change(x, dp = 1, fill = "", form = "f", ...)

mcomma_change(x, dp = 0, fill = "", form = "f", ...)

bdollar(x, dp = 1, fill = "", form = "f", ...)

bcomma(x, dp = 0, fill = "", form = "f", ...)

bdollar_change(x, dp = 1, fill = "", form = "f", ...)

bcomma_change(x, dp = 0, fill = "", form = "f", ...)
}
\arguments{
\item{x}{Numeric variable to format}

\item{dp}{The number of decimal points to display}

\item{fill}{If there are missing values, how should they be displayed?}

\item{...}{arguments passed to \code{format}.}
}
\description{
More comprehensive versions of \code{\link{scales}} functions for displaying numeric values in human-readable format.
These functions better display negative values and allow for improved control over decimal places in comparison to the versions from \code{scales}.
}
\section{Available Functions}{

The following functions are currently available for use:
\describe{
  \item{\code{dollar}}{For dollar values}
  \item{\code{percent}}{For decimal percentages}
  \item{\code{dollar_change}}{Changes in dollar values}
  \item{\code{percent_change}}{Changes in percentages}
  \item{\code{comma_change}}{Changes in numeric values}
  \item{\code{kdollar}}{Thousands of dollars}
  \item{\code{kcomma}}{Thousands}
  \item{\code{kdollar_change}}{Changes in thousands of dollars}
  \item{\code{kcomma_change}}{Changes in thousands}
  \item{\code{mdollar}}{Millions of dollars}
  \item{\code{mcomma}}{Millions}
  \item{\code{mdollar_change}}{Changes in mdollars}
  \item{\code{mcomma_change}}{Changes in millions}
  \item{\code{bdollar}}{Billions of dollars}
  \item{\code{bcomma}}{Billions}
  \item{\code{bdollar_change}}{Changes in bdollars}
  \item{\code{bcomma_change}}{Changes in billions}
}
If there are others that you want to be implemented, let me (Bert) know
}

\examples{
dollar(1000000)
dollar(c(-2000000, 15000000))
comma(100000)
comma(c(-20000, 15000000))
percent(c(-0.20, 0.53))
dollar_change(c(-200, 15))
percent_change(c(-0.20, 0.53))
comma_change(c(-2000000, 15000000))
kdollar(1000000)
kdollar(c(-2000000, 15000000))
kdollar(100000)
kdollar(c(-20000, 15000000))
kdollar_change(c(-200000, 15000))
kcomma_change(c(-2000000, 15000000))
mdollar(1000000)
mdollar(c(-2000000, 15000000))
mdollar(100000)
mdollar(c(-20000, 15000000))
mdollar_change(c(-2000000, 150000))
mcomma_change(c(-2000000, 15000000))
bdollar(1000000)
bdollar(c(-2000000, 15000000))
bdollar(100000)
bdollar(c(-20000, 15000000))
bdollar_change(c(-2000000, 150000))
bcomma_change(c(-2000000, 15000000))
}
