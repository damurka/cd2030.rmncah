% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_derived_coverage.R
\name{calculate_derived_coverage}
\alias{calculate_derived_coverage}
\title{Generate Coverage Data with Derived Denominators}
\usage{
calculate_derived_coverage(.data, indicator, base_year, region = NULL)
}
\arguments{
\item{.data}{A \code{cd_population_metrics} object containing indicator values and DHIS2 population.}

\item{indicator}{A character string specifying the indicator to calculate coverage for.}

\item{base_year}{Integer. The year from which denominator proportions are derived.}
}
\value{
A \code{cd_derived_coverage} tibble with columns for old and new coverage estimates.
}
\description{
Calculates trend-adjusted and subnationally-redistributed coverage estimates
using the DTP1-derived denominator logic described in CD2030. It estimates
coverage over time based on changes in DHIS2 population counts, while preserving
subnational proportions from the base year.
}
\details{
This allows estimating:
\itemize{
\item Trends over time in coverage
\item Subnational inequities
}
}
\examples{
calculate_derived_coverage(dhis_data, "penta1", 2019)

}
