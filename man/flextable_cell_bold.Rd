% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReporteRs functions.R
\name{flextable_cell_bold}
\alias{flextable_cell_bold}
\alias{ft_cell_bold}
\title{Bold cells of a flextable}
\usage{
flextable_cell_bold(ft, rows = ft$numrow, cols = 1:ft$numcol,
  colour = marketview::mvl_text)

ft_cell_bold(ft, rows = ft$numrow, cols = 1:ft$numcol,
  colour = marketview::mvl_text)
}
\arguments{
\item{ft}{The flextable object to highlight}

\item{rows}{The index of the rows that should be highlighted}

\item{cols}{The columns of the cells that should be highlighted}

\item{colour}{The colour that the bold text should have}
}
\value{
ft A modified FlexTable with the specified cells highlighted with bold text in the given colour
}
\description{
A function to highlight particular cells of a FlexTable with bold text and colour
}
\examples{
df_ex <- head(cars)
ft_ex <- flextable_teal(df_ex)
ft_ex <- flextable_cell_bold(ft_ex, which(df_ex$speed > 7), "speed", "forestgreen")
}
