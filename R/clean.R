#' Clean and tidy raw GEM data
#' @export
#' @import dplyr
#' @param x a data frame of raw GEM microdata

clean_gem <- function(x) {
  if (any(grepl("16", names(x)))) {
    x <- x %>%
      rename(TEAyy = TEA16, TEAyyOPP = TEA16OPP, TEAyyNEC = TEA16NEC)
  }
  vars <- c("country", "yrsurv", "CAT_GCR1", "CAT_GCR2", "gender",
            "CAT_Income3", "CAT_region6", "RPT2019",
            "WEIGHT_L", "age", "age5c", "age7c", "age9c",
            "GEMHHINC", "IPACT_ALL", "occuself", "GEMOCCU",
            "omexport", "TEAISIC4_1D", "EB_ISIC4_1D",
            "sunowjob", "suyr5job", "omnowjob", "omyr5job",
            "suexport", "busang", "BAFUNDUS", "BUSANGVL", "barel",
            "SUBOANW", "BABYBUSO",
            "barel_5c", "easystart", "nbgoodc", "nbstatus", "nbmedia",
            "TEAEXPST", "GEMEDUC", "UNEDUC", "hhsize",
            #          "TEA16", "TEA16OPP", "TEA16NEC",
            "TEAyy", "TEAyyOPP", "TEAyyNEC", "TEAyySTA", "TEAyyJNW",
            "TEAJOBEX", "TEAEXPST", "TEAyyNPM", "TEAISIC_6C",
            "knowent", "opport", "suskill", "fearfail",
            "FUTSUPNO", "ESTBBUSO", "DISCENyy", "EXIT_RS3")
  out <- x %>%
    select(any_of(vars)) %>%
    mutate(
      UNEDUC = as.character(UNEDUC),
      BUSANGVL = as.character(BUSANGVL),
      country = as.character(country)
    ) %>%
    tibble::as_tibble() %>%
    filter(!is.na(gender))
  return(out)
}

#' @rdname clean_gem
#' @export
#' @param x a list of GEM data frames

bind_gem <- function(x) {

  out <- x %>%
    map(clean_gem) %>%
    bind_rows(.id = "year")

}

#' @export
#' @rdname clean_gem

fct_gem <- function(x) {

  # Fix factor levels
  uneduc_levels <- c("Pre-primary education",
                     "Primary education or first stage of basic education",
                     "Lower  secondary or second stage of basic education",
                     "(Upper) secondary education",
                     "Post-secondary non-tertiary education",
                     "First stage of tertiary education",
                     "Second stage of tertiary education", -2, 7, 8)

  income_levels <- c("Low/Lower-middle Income", "Upper-middle Income", "High Income")

#  out <- x %>%
#    left_join(country_income_lkup, by = c("country" = "Economy"))

  out <- x %>%
    mutate(
      # year = parse_number(year),
      UNEDUC = gsub("-1", NA, UNEDUC),
      uneduc = factor(UNEDUC, levels = uneduc_levels),
      income_level = factor(CAT_Income3, levels = income_levels),
      Region = as.character(CAT_region6),
      TEAyy = tolower(TEAyy),
      age9c = factor(age9c,
                     levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-120", NA)),
      TEAyyNEC = tolower(TEAyyNEC),
      TEAyyOPP = tolower(TEAyyOPP)
    )

  return(out)
}
