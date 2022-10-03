#' Clean and tidy raw GEM data
#' @export
#' @import dplyr
#' @param x a data frame of raw GEM microdata

clean_gem <- function(x) {
  if (any(grepl("16", names(x)))) {
    x <- x %>%
      rename(TEAyy = TEA16, TEAyyOPP = TEA16OPP, TEAyyNEC = TEA16NEC)
  }
  if (any(grepl("20", names(x)))) {
    x <- x %>%
      rename(TEAyy = TEA20)
  }
  vars_2019 <- c("country", "yrsurv", "CAT_GCR1", "CAT_GCR2", "gender",
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
  vars_2021 <- c("WEIGHT_L",
                 "gender",
                 "Country_name",
                 "REGION_4",
                 "WBincREV",
                 "TEA20  ",
                 "TEA20MOT1yes",
                 "TEA20MOT2yes",
                 "TEA20MOT3yes",
                 "TEA20MOT4yes",
                 "FUTSUPNO ",
                 "SUBOA",
                 "BABYBUSO",
                 "ESTBBUSO",
                 "DISCEN20",
                 "EXIT_RS",
                 "TEAISIC4_6",
                 "TEA20JNW",
                 "TEA20J5Y",
                 "TEANEWPROD",
                 "TEAEXPST",
                 "TEA20MKSC",
                 "age_3",
                 "GEMEDUC ",
                 "GEMHHINC",
                 "OPPORT20",
                 "SUSKIL20",
                 "FRFAIL20",
                 "KNOWEN20",
                 "EASYST20",
                 "NBGOOD20",
                 "NBSTAT20",
                 "NBMEDI20",
                 "BUSANG20",
                 "BAFUNDUS",
                 "BAREL_O",
                 "CPKNSTART1",
                 "CPKNSTOP1",
                 "FUTSUP_CP_SM",
                 "FUTSUP_CP_HI",
                 "TEACRSTART",
                 "TEACPNEWOPP",
                 "TEACRGROW",
                 "TEACPGOVRES",
                 "EB_CRSTART",
                 "EB_CPNEWOPP",
                 "EB_CRGROW",
                 "EB_CPGOVRES"
  )
  vars_2022 <- c(
    "WEIGHT_L",
    "Country_name",
    "REGION_4",
    "WBinc3rev",
    "WBinc_3",
    "WBINC_4",
    "female",
    "age3c",
    "GEMEDUC",
    "GEMHHINC",
    "TEA21",
    "TEA21MOT1yes",
    "TEA21MOT2yes",
    "TEA21MOT3yes",
    "TEA21MOT4yes",
    "FUTSUPNO",
    "SUBOANW",
    "BABYBUSO",
    "ESTBBUSO",
    "DISCEN21",
    "EXIT_RS7",
    "TEAISIC4_6",
    "TEA21JNW",
    "TEA21J5Y",
    "TEANEWPROD",
    "TEAEXPST",
    "TEA21MKSC",
    "BUSANG21",
    "BAFUNDUS",
    "BAREL_O",
    "OPPORT21",
    "SUSKIL21",
    "FRFAIL21",
    "KNOWEN21",
    "EASYST21",
    "NBGOOD21",
    "NBSTAT21",
    "NBMEDI21",
    "TEACPNEWOPP",
    "TEACPGOVRES",
    "TEACPTECH1",
    "TEACPTECH2",
    "EB_CPNEWOPP",
    "EB_CPGOVRES",
    "EB_CPTECH1",
    "EB_CPTECH2",
    "NES_P01_MEAN10",
    "NES_P02_MEAN10",
    "NES_P03_MEAN10",
    "NES_P04_MEAN10",
    "NES_P05_MEAN10",
    "NES_P06_MEAN10"
  )

  out <- x %>%
    select(any_of(c(vars_2019, vars_2021, vars_2022))) %>%
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

fct_gem_2019 <- function(x) {

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
      age9c = factor(
        age9c,
        levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-120", NA)
      ),
      TEAyyNEC = tolower(TEAyyNEC),
      TEAyyOPP = tolower(TEAyyOPP)
    )

  return(out)
}


#' @export
#' @rdname clean_gem

fct_gem_2021 <- function(x) {

  # Fix factor levels
  uneduc_levels <- c("Pre-primary education",
                     "Primary education or first stage of basic education",
                     "Lower  secondary or second stage of basic education",
                     "(Upper) secondary education",
                     "Post-secondary non-tertiary education",
                     "First stage of tertiary education",
                     "Second stage of tertiary education", -2, 7, 8)

  out <- x %>%
    mutate(
      # year = parse_number(year),
      UNEDUC = gsub("-1", NA, UNEDUC),
      uneduc = factor(UNEDUC, levels = uneduc_levels),
      Region = REGION_4,
      TEAyy = tolower(TEAyy),
#      TEAyyNEC = tolower(TEAyyNEC),
#      TEAyyOPP = tolower(TEAyyOPP)
    )

  return(out)
}


#' @export
#' @rdname clean_gem

fct_gem_2022 <- function(x) {

  # Fix factor levels
  uneduc_levels <- c("Pre-primary education",
                     "Primary education or first stage of basic education",
                     "Lower  secondary or second stage of basic education",
                     "(Upper) secondary education",
                     "Post-secondary non-tertiary education",
                     "First stage of tertiary education",
                     "Second stage of tertiary education", -2, 7, 8)

  out <- x %>%
    mutate(
      # year = parse_number(year),
      UNEDUC = gsub("-1", NA, UNEDUC),
      uneduc = factor(UNEDUC, levels = uneduc_levels),
      Region = REGION_4,
      #      TEAyyNEC = tolower(TEAyyNEC),
      #      TEAyyOPP = tolower(TEAyyOPP)
    )

  return(out)
}
