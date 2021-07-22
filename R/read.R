#' Read GEM data from SAV files
#' @export
#' @param file path to a SAV file
#' @param ... arguments passed to \code{\link[haven]{read_sav}}
#' \dontrun{
#' files <- fs::dir_ls("~/Data/gem", regexp = ".sav$")
#' gem_data <- read_gem_data(files[1])
#' }

read_gem_data <- function(file, ...) {
  # Note that haven is faster but returns fewer columns
  file %>%
#    haven::read_sav(...)
    foreign::read.spss(to.data.frame = TRUE) %>%
    tibble::as_tibble()
}
