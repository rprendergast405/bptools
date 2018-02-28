#' Comparison Plot of TLD Item Performance
#'
#' @param con ODBC connection to bespoke
#' @param product_tbl Table of products to consider
#' @param group_name Name for the plot title
#' @param index Should AWUs be indexed to the first week?
#' @param align_weeks Should the weeks be standardised to 'Week 1', 'Week 2', etc? Otherwise uses seqpromo_week
#'
#' @return
#' @export tld_comparison_plot
#'
#' @examples
tld_comparison_plot <- function(con, product_tbl, group_name, align_weeks = FALSE, index = FALSE, promo_week = TRUE) {

  # partial function to use with purrr::pmap
  tld_con <- purrr::partial(tld_sales_summary, con = con, group_name = "", promo_week = promo_week)

  # nest the original table and get the tld sales data for each group
  product_tbl <- tidyr::nest(product_tbl, codes, .key = "codes")
  product_tbl <- dplyr::mutate(product_tbl, codes = purrr::map(codes, ~as.integer(.$codes)))


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
  sales_df <- dplyr::select(sales_df, item_name, awu_df)
  sales_df <- tidyr::unnest(sales_df)
  sales_df <- dplyr::group_by(sales_df, item_name)
  sales_df <- dplyr::mutate(sales_df,
                            week_num = ceiling(as.numeric(w_date - min(w_date)) / 7) + 1,
                            week_name = paste("Week", week_num)
  )
  sales_df <- dplyr::ungroup(sales_df)

  if (!promo_week) {
    sales_df <- dplyr::mutate(sales_df, SEQPROMO_WEEK = SEQWEEK)
  }

  if (align_weeks) {
    sales_df <- dplyr::mutate(sales_df, week_name = forcats::fct_reorder(week_name, week_num))
  } else {
    sales_df <- dplyr::mutate(sales_df, week_name = as.character(SEQPROMO_WEEK))
  }
  sales_df <- dplyr::group_by(sales_df, item_name, week_name)
  sales_df <- dplyr::summarise(sales_df, quantity = sum(QUANTITY, na.rm = TRUE), sites = max(SITES, na.rm = TRUE))
  sales_df <- dplyr::mutate(sales_df, awu = quantity / sites)

  if (index) sales_df <- dplyr::mutate(sales_df, awu = awu / awu[1])

  # Make a df for labeling the lines
  label_df <- dplyr::group_by(sales_df, item_name)
  label_df <- dplyr::slice(label_df, ifelse(index, n(), 1))

  value_labels <- dplyr::group_by(sales_df, item_name)
  value_labels <- dplyr::slice(value_labels, c(1, n()))
  value_labels <- dplyr::mutate(value_labels, text =  ifelse(row_number() == 1,
                                                             paste(scales::comma(round(awu)), "            "),
                                                             paste("            ", scales::comma(round(awu)))))


  # Construct the plot

  week_lab <- case_when(promo_week ~ "Promo Week",
                        TRUE ~ "Week")

  p <- ggplot2::ggplot(data = sales_df,
                       mapping = ggplot2::aes(x = week_name, y = awu, colour = item_name)) +
    ggplot2::geom_line(mapping = ggplot2::aes(group = item_name),
                       size = 1, show.legend = FALSE) +
    scale_colour_mvl(palette = "Highlight", highlight = mvl_plum) +
    theme_mvl(base_family = "hn") +
    ggrepel::geom_label_repel(data = label_df,
                             mapping = ggplot2::aes(label = item_name),
                             family = "hnb", size = 3, alpha = 0.75,
                             direction = "y", label.size = NA,
                             show.legend = FALSE) +
    ggplot2::geom_text(data = value_labels,
                             mapping = ggplot2::aes(label = text),
                             family = "hnb", size = 3,
                             show.legend = FALSE) +
    ggplot2::labs(title = paste(group_name, "AWUs", ifelse(index, "(Indexed)", "")),
                  y = "Average Weekly Units",
                  x = week_lab) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  if (index) {
    p <- p  +
      ggplot2::scale_y_continuous(labels = marketview::percent, limits = c(0, NA))
  } else {
    p <- p  +
      ggplot2::scale_y_continuous(labels = scales::comma, limits = c(0, NA))
  }

  seqwk_labels <- function(x) {
    stopifnot(all(nchar(x) == 6))
    return(paste0(substr(x, 1, 4), "\nw", substr(x, 5, 6)))
  }

  # Label the seqweeks a bit more compact
  if (!align_weeks) {
    p <- p + scale_x_discrete(labels = seqwk_labels)
  }

  return(p)
}
