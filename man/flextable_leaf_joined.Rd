% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReporteRs functions.R
\name{flextable_leaf_joined}
\alias{flextable_leaf_joined}
\alias{ft_leaf_joined}
\title{Create a FlexTable with Multiple Header Rows}
\usage{
flextable_leaf_joined(df, pr, hr, size = 12, padding = size/2)

ft_leaf_joined(df, pr, hr, size = 12, padding = size/2)
}
\arguments{
\item{df}{A \code{data.frame} of values}

\item{pr}{Pseudo-rows: Indices of the header cells in df to be merged}

\item{hr}{Header Rows: Indices of the rows to apply header formatting}

\item{size}{Font size in pt}

\item{padding}{Cell padding}
}
\value{
df_ft A FlexTable of the df with formatting
}
\description{
Format and print a joined FlexTable
}
