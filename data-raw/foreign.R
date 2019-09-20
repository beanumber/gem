library(tidyverse)
library(fs)

sav <- dir_ls("~/Data/gem/", regexp = ".sav$")

gem <- sav %>%
  map(foreign::read.spss, to.data.frame = TRUE) %>%
  map(as_tibble)

names(gem) <- parse_number(names(gem))

save(gem, file = path("~/Data/gem", "gem_2015-2018.rda"), compress = "xz")

gem_1718 <- gem[[1]]

save(gem_1718, file = path("~/Data/gem", "gem_2017-2018.rda"), compress = "xz")

