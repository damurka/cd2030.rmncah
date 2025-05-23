% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_sum.outliers_summary.R
\name{tbl_sum.cd_district_outliers_summary}
\alias{tbl_sum.cd_district_outliers_summary}
\title{Summary for \code{cd_district_outliers_summary}}
\usage{
\method{tbl_sum}{cd_district_outliers_summary}(x, ...)
}
\arguments{
\item{x}{A \code{cd_district_outliers_summary} object containing the percentage of districts without extreme outliers
for each indicator and year.}

\item{...}{Additional arguments for compatibility with S3 methods.}
}
\value{
A character vector describing the purpose and content of the data, highlighting the percentage of districts
with no extreme outliers for each indicator annually.
}
\description{
Provides a summary for the \code{cd_district_outliers_summary} object, indicating the percentage of districts
without extreme outliers for each indicator by year. This summary supports monitoring data quality across
districts by showing the proportion of non-extreme values across all indicators.
}
