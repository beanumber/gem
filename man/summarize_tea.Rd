% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tea.R
\name{summarize_tea}
\alias{summarize_tea}
\title{Summarize TEA}
\usage{
summarize_tea(.data, ...)
}
\arguments{
\item{.data}{A data frame, data frame extension (e.g. a tibble), or a
lazy data frame (e.g. from dbplyr or dtplyr). See \emph{Methods}, below, for
more details.}

\item{...}{<\code{\link[dplyr:dplyr_data_masking]{data-masking}}> Name-value pairs of summary
functions. The name will be the name of the variable in the result.

The value can be:
\itemize{
\item A vector of length 1, e.g. \code{min(x)}, \code{n()}, or \code{sum(is.na(y))}.
\item A vector of length \code{n}, e.g. \code{quantile()}.
\item A data frame, to add multiple columns from a single expression.
}}
}
\description{
Summarize TEA
}
