#' Summarize TEA
#' @inheritParams dplyr::summarize
#' @export

summarize_tea <- function(.data, ...) {
  .data %>%
    summarize(N = n(),
              N_wgt = sum(WEIGHT_L, na.rm = TRUE),
              tea_yy = sum(WEIGHT_L * (TEAyy == "yes"), na.rm = TRUE),
              ...
    ) %>%
    mutate(tea_yy_pct = tea_yy / N_wgt,
           se = sqrt(tea_yy_pct * (1 - tea_yy_pct) / N_wgt),
           lower = tea_yy_pct - 1.96 * se,
           upper = tea_yy_pct + 1.96 * se)
}
