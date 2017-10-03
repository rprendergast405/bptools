#' Comparison Plot of TLD Item Performance
#'
#' @param con
#' @param product_tbl
#' @param group_name
#' @param index
#'
#' @return
#' @export tld_comparison_plot
#'
#' @examples
tld_comparison_plot <- function(con, product_tbl, group_name, align_weeks = FALSE, index = FALSE) {

  # partial function to use with purrr::pmap
  tld_con <- purrr::partial(tld_sales_summary, con = con, group_name = "")

  # Get the AWU summary data for each product group from the table
  sales_df <- dplyr::mutate(
    product_tbl,
    awu_df = purrr::pmap(list(
      codes,
      start_week,
      end_week
    ),
    tld_con)
  )

  # Process the data - Standardise the week numbers to align in the plot
  sales_df <- dplyr::select(sales_df, group, awu_df)
  sales_df <- tidyr::unnest(sales_df)
  sales_df <- dplyr::group_by(sales_df, group)
  sales_df <- dplyr::mutate(sales_df,
                            week_num = ceiling(as.numeric(w_date - min(w_date)) / 7) + 1,
                            week_name = paste("Week", week_num)
  )
  sales_df <- dplyr::ungroup(sales_df)

  if (align_weeks) {
    sales_df <- dplyr::mutate(sales_df, week_name = forcats::fct_reorder(week_name, week_num))
  } else {
    sales_df <- dplyr::mutate(sales_df, week_name = as.character(SEQPROMO_WEEK))
  }
  sales_df <- dplyr::group_by(sales_df, group, week_name)
  sales_df <- dplyr::summarise(sales_df, awu = sum(AWU))

  if (index) sales_df <- dplyr::mutate(sales_df, awu = awu / awu[1])

  # Make a df for labeling the lines
  label_df <- dplyr::group_by(sales_df, group)
  label_df <- dplyr::slice(sales_df, ifelse(index, n(), 1))

  # Construct the plot
  p <- ggplot2::ggplot(data = sales_df,
                       mapping = ggplot2::aes(x = week_name, y = awu, colour = group)) +
    ggplot2::geom_line(mapping = ggplot2::aes(group = group),
                       size = 1, show.legend = FALSE) +
    scale_colour_mvl(palette = "Highlight", highlight = mvl_plum) +
    theme_mvl() +
    ggrepel::geom_text_repel(data = label_df,
                             mapping = ggplot2::aes(label = group),
                             family = "hnb", size = 3,
                             show.legend = FALSE) +
    ggplot2::labs(title = paste(group_name, "AWUs", ifelse(index, "(Indexed)", "")),
                  y = "Average Weekly Units",
                  x = "Promo Week") +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  if (index) {
    p <- p  +
      ggplot2::scale_y_continuous(labels = marketview::percent)
  } else {
    p <- p  +
      ggplot2::scale_y_continuous(labels = scales::comma)
  }

  return(p)
}
