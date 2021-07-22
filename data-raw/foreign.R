library(tidyverse)
library(fs)

sav <- dir_info("~/Data/gem/", regexp = ".sav$") %>%
  filter(str_detect(path, "Indiv")) %>%
  filter(str_detect(path, "-", negate = TRUE)) %>%
  mutate(
    gem_year = parse_number(path),
    gem_name = paste0("gem", gem_year),
    file_rda = paste0("gem_name", ".rda")
  )

# gem2015 <- gem::read_gem_data(sav$path[1])
# lobstr::obj_size(gem2015) / 1024^2

gem <- sav %>%
  pull(path) %>%
#  tail(2) %>%
  map(gem::read_gem_data) %>%
  map(gem::clean_gem) %>%
  set_names(sav$gem_name)

# map2(names(gem), gem, ~assign(.x, .y))

assign(names(gem)[1], gem[[1]])
assign(names(gem)[2], gem[[2]])
assign(names(gem)[3], gem[[3]])
assign(names(gem)[4], gem[[4]])
assign(names(gem)[5], gem[[5]])
assign(names(gem)[6], gem[[6]])

# map(sav$gem_name, usethis::use_data)

usethis::use_data(
  gem2015, gem2016, gem2017,
  gem2018, gem2019, gem2020,
  overwrite = TRUE
)




