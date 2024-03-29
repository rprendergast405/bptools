% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/old_functions.R
\name{save_table}
\alias{save_table}
\title{Save data.frame to csv & xlsx.}
\usage{
save_table(x, write_dir, name, tab_num = 1, xlsx = TRUE,
  xls_name = "all_tables.xlsx")
}
\arguments{
\item{x}{The table to save}

\item{write_dir}{The directory in which the tables should be saved}

\item{name}{The name of the saved object}

\item{tab_num}{A number that preceeds the name}
}
\value{
Saves the table to file
}
\description{
Saves copies of a data frame to a given directory.
}
