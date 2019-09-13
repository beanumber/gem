library(tidyverse)
library(fs)

## Notes

# - minimum `N` is 2000
# - Spain has 10 regions with 2000 in each

## Loading the SPSS data

sav <- dir_ls("~/Data/gem/", regexp = ".sav$")

gem <- sav %>%
  map(foreign::read.spss, to.data.frame = TRUE) %>%
  map(as_tibble)

names(gem) <- parse_number(names(gem))

save(gem, file = path("~/Data/gem", "gem_2015-2018.rda"), compress = "xz")
