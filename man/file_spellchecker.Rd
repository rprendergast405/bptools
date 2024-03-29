% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_spellchecker.R
\name{file_spellchecker}
\alias{file_spellchecker}
\title{Check for Spelling Errors}
\usage{
file_spellchecker(file_connection, quote_type = c("both", "double", "single"),
  output = c("counts", "locations", "both"), ...)
}
\arguments{
\item{file_connection}{Location of the file to spellcheck}

\item{quote_type}{Should}

\item{...}{Arguments to which_misspelled}
}
\description{
A function that will check for spelling errors in strings manually entered in a script.
This returns two data.frames, one with a row for each string and a column for each mistake, the second is a summary of each unique mistake and the number of times it appears in the first df.
(Note the second df only counts one occurrences of each error in each string, hence it gives the number of strings a particular error occurs in, but not necessarily the total number of errors to be fixed)
}
