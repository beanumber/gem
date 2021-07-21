#' Summarize TEA
#' @inheritParams dplyr::summarize
#' @export

summarize_tea <- function(.data, ...) {
  .data %>%
    summarize(
      N = n(),
      N_wgt = sum(WEIGHT_L, na.rm = TRUE),
#      tea_yy_uwgt = sum(TEAyy == "yes", na.rm = TRUE),
      tea_yy = sum(WEIGHT_L * (TEAyy == "yes"), na.rm = TRUE),
      ...
    ) %>%
    mutate(
      tea_yy_pct = tea_yy / N_wgt,
      se = sqrt(tea_yy_pct * (1 - tea_yy_pct) / N_wgt),
      lower = tea_yy_pct - 1.96 * se,
      upper = tea_yy_pct + 1.96 * se,
#      tea_yy_pct_wgt = tea_yy_wgt / N_wgt,
#      se_wgt = sqrt(tea_yy_pct_wgt * (1 - tea_yy_pct_wgt) / N),
#      lower_wgt = tea_yy_pct_wgt - 1.96 * se_wgt,
#      upper_wgt = tea_yy_pct_wgt + 1.96 * se_wgt
    )
}


#' Theming
#' @export
#' @inheritParams ggplot2::theme_grey
#' @importFrom ggplot2 %+replace%
#' @examples
#' if (require(ggplot2)) {
#' p <- ggplot(mtcars, aes(x = hp, y = mpg, color = factor(cyl))) +
#'   geom_point() + facet_wrap(~ am) + geom_smooth()
#' p + theme_grey()
#' p + theme_gem(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
#' p + theme_gem(panel.grid.minor = element_blank())
#'
#' if (require(dplyr)) {
#' data(gem2018)
#' countries <- gem2018 %>%
#'   group_by(country) %>%
#'   summarize_tea()
#' q <- ggplot(countries, aes(x = country, y = tea_yy_pct)) +
#'   geom_col()
#' q + theme_gem()
#'   # workaround!
#' q + theme_gem(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8))
#' }
#' }

theme_gem <- function(base_size = 12, base_family = "Helvetica", ...) {
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      legend.position  = "top",
      axis.text        = ggplot2::element_text(size = ggplot2::rel(0.8)),
      axis.ticks       = ggplot2::element_line(colour = "black"),
      legend.key       = ggplot2::element_rect(colour = "grey80"),
      panel.background = ggplot2::element_rect(fill = "whitesmoke", colour = NA),
      panel.border     = ggplot2::element_rect(fill = NA, colour = "grey50"),
      panel.grid.major = ggplot2::element_line(colour = "grey80", size = 0.2),
      panel.grid.minor = ggplot2::element_line(colour = "grey92", size = 0.5),
      strip.background = ggplot2::element_rect(fill = "grey80", colour = "grey50", size = 0.2)
    ) %+replace%
    ggplot2::theme(...)
}
