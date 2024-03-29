% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tea.R
\name{theme_gem}
\alias{theme_gem}
\title{Theming}
\usage{
theme_gem(base_size = 12, base_family = "Helvetica", ...)
}
\arguments{
\item{base_size}{base font size, given in pts.}

\item{base_family}{base font family}
}
\description{
Theming
}
\examples{
if (require(ggplot2)) {
p <- ggplot(mtcars, aes(x = hp, y = mpg, color = factor(cyl))) +
  geom_point() + facet_wrap(~ am) + geom_smooth()
p + theme_grey()
p + theme_gem(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
p + theme_gem(panel.grid.minor = element_blank())

if (require(dplyr)) {
data(gem2018)
countries <- gem2018 \%>\%
  group_by(country) \%>\%
  summarize_tea()
q <- ggplot(countries, aes(x = country, y = tea_yy_pct)) +
  geom_col()
q + theme_gem()
  # workaround!
q + theme_gem(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8))
}
}
}
