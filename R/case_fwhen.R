#' Vectorised if returning an ordered result
#'
#' This function allows you to vectorise multiple if and else if statements. It is an R equivalent of the SQL CASE WHEN statement.
#' An update from the dplyr implementation as it returns the result as an ordered factor in the order given by the formulae
#'
#' @inheritParams dplyr::case_when
#'
#' @export case_fwhen

case_fwhen <- function(...) {
  formulas <- rlang::dots_list(...)

  formula_chr <- lapply(formulas, function(x) as.character(x)[3])

  formula_chr <- do.call(rbind, formula_chr)

  formula_chr <- as.character(formula_chr)

  result <- dplyr::case_when(...)

  result <- factor(result, levels = formula_chr)

  return(result)
}
