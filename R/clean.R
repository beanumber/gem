#' Clean and tidy raw GEM data
#' @export
#' @import dplyr
#' @param x a data frame of raw GEM microdata

clean_gem_year <- function(x) {
  if (any(grepl("16", names(x)))) {
    x <- x %>%
      rename(TEAyy = TEA16, TEAyyOPP = TEA16OPP, TEAyyNEC = TEA16NEC)
  }
  vars <- c("country", "CAT_GCR1", "CAT_GCR2", "gender",
            "WEIGHT_L", "age", "age7c", "age9c",
            "GEMHHINC", "IPACT_ALL", "occuself", "GEMOCCU",
            "omexport", "TEAISIC4_1D", "EB_ISIC4_1D",
            "sunowjob", "suyr5job", "omnowjob", "omyr5job",
            "suexport", "busang", "BAFUNDUS", "BUSANGVL", "barel",
            "TEAEXPST", "GEMEDUC", "UNEDUC", "hhsize",
            #          "TEA16", "TEA16OPP", "TEA16NEC",
            "TEAyy", "TEAyyOPP", "TEAyyNEC")
  out <- x %>%
    select(one_of(vars)) %>%
    mutate(UNEDUC = as.character(UNEDUC),
           BUSANGVL = as.character(BUSANGVL),
           country = as.character(country)) %>%
    tibble::as_tibble() %>%
    filter(!is.na(gender))
  return(out)
}

#' @rdname clean_gem_year
#' @export
#' @param x a list of GEM data frames

clean_gem <- function(x) {

  out <- x %>%
    map(clean_gem_year) %>%
    bind_rows(.id = "year")

  # Fix factor levels
  uneduc_levels <- c("Pre-primary education",
                     "Primary education or first stage of basic education",
                     "Lower  secondary or second stage of basic education",
                     "(Upper) secondary education",
                     "Post-secondary non-tertiary education",
                     "First stage of tertiary education",
                     "Second stage of tertiary education", -2, 7, 8)

  out <- out %>%
    mutate(year = parse_number(year),
           UNEDUC = gsub("-1", NA, UNEDUC),
           uneduc = factor(UNEDUC, levels = uneduc_levels),
           TEAyy = tolower(TEAyy))

  out <- out %>%
    left_join(country_income_lkup, by = c("country" = "Economy"))

  return(out)
}
