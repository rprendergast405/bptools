#' Check for Spelling Errors
#'
#'A function that will check for spelling errors in strings manually entered in a script.
#'This returns two data.frames, one with a row for each string and a column for each mistake, the second is a summary of each unique mistake and the number of times it appears in the first df.
#'(Note the second df only counts one occurrences of each error in each string, hence it gives the number of strings a particular error occurs in, but not necessarily the total number of errors to be fixed)
#'
#' @param file_connection Location of the file to spellcheck
#' @param quote_type Should
#' @param ... Arguments to which_misspelled
#'
#' @export file_spellchecker
file_spellchecker <- function(file_connection, quote_type = c("both", "double", "single"), output = c("counts", "locations", "both"), ...){

  quote_type <- match.arg(quote_type)
  output <- match.arg(output)

  #Read the file
  flines <- readLines(file_connection)

  #Match all strings encased in single or double quotations
  #i.e. (those that were manually entered by the user)
  dq_strings <- stringr::str_match(flines, "\"(.*?)\"")[, 1]
  sq_strings <- stringr::str_match(flines, "\'(.*?)\'")[, 1]

  #Create strings to check
  if (quote_type == 'both') {
    strings <- c(sq_strings[!is.na(sq_strings)], dq_strings[!is.na(dq_strings)])
  }
  if (quote_type == 'double') {
    strings <- dq_strings[!is.na(dq_strings)]
  }
  if (quote_type == 'single') {
    strings <- sq_strings[!is.na(sq_strings)]
  }

  #Match all acronyms, create a filter to remove these from spell check
  acronyms <- stringr::str_match(strings, "[A-Z]{2,}")
  acronyms <- unique(acronyms[!is.na(acronyms)])
  acronym_filt <- paste(acronyms, collapse = "|")

  #Find all spelling errors, use supply dots to change dictionaries
  cat("Checking for spelling errors\n")
  errors <- gsub(acronym_filt, "", strings)
  errors <- sapply(errors, qdap::which_misspelled, ...)

  errors <- errors[(1:length(errors))[as.logical(sapply(errors, function(x){!is.null(x)}))]]

  if (length(errors) == 0) stop("Your script contains no errors; congratulations")

  #Find the maximum number of mistakes in a single string
  max_mistakes <- max(sapply(errors, length))

  #Create data.frame listing errors
  error_df <- suppressWarnings(data.frame(names(errors),  t(data.table::as.data.table(errors))[, 1:max_mistakes]))
  error_df <- setNames(error_df, c('strings', paste0('error_', 1:max_mistakes)))

  #If there is more than one error column, remove duplicate errors
  if (max_mistakes > 1) {
    for (i in 2:max_mistakes) {
      error_df <- remove_duplicate_errors(dat = error_df,  e_col = paste0('error_', i))
    }
  }

  #Create a summarised data.frame of the count of unique string x error combinations
  summarised_errors <- lapply(paste0('error_', 1:max_mistakes), error_summarise, dat = error_df)
  summarised_errors <- lapply(summarised_errors, function(x) {setNames(x, c('error', 'count'))})

  summarised_errors <- dplyr::bind_rows(summarised_errors)
  summarised_errors <- dplyr::group_by(summarised_errors, error)
  summarised_errors <- dplyr::summarise(summarised_errors, count = sum(count))
  summarised_errors <- dplyr::filter(summarised_errors, !is.na(error))
  summarised_errors <- dplyr::arrange(summarised_errors, dplyr::desc(count))

  n_errors <- sum(summarised_errors$count)

  output_dat <- list(error_df, summarised_errors)
  if (output == "counts") {
    cat(paste0(n_errors, " spelling errors were identified (use output = \"locations\" to see the full context for each error):\n"))
    output_dat <- summarised_errors
  }

  if (output == "locations") {
    output_dat <- error_df
  }

  return(output_dat)

}

#Helper functions

error_summarise <- function(dat, evar_str) {

  dat$err_count <- 1
  dat <- dplyr::group_by_(dat, evar_str)
  dat <- dplyr::summarise(dat, error_count = sum(err_count))
  dat <- dplyr::arrange(dat, dplyr::desc(error_count))

  return(dat)
}

remove_duplicate_errors <- function(dat, e_col) {

  dat[dat[, 'error_1'] == dat[, e_col], e_col] <- NA

  return(dat)

}
