# NES

nes21 <- here::here("data-raw", "GEM 2021 NES NATIONAL LEVEL.sav") |>
  foreign::read.spss(to.data.frame = TRUE) |>
  tibble::as_tibble()

usethis::use_data(nes21, overwrite = TRUE)
