% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surveytools.R
\name{chisquaretester2}
\alias{chisquaretester2}
\title{Chi square comparison, version 2}
\usage{
chisquaretester2(control_stimulus, stimulus, column, output = "csv")
}
\arguments{
\item{control_stimulus}{dataframe with control group
stimulus dataframe with treatment group to compare to control_stimulus
column the column to be read from both spread sheets
output "txt" for verbose text output, "csv" for csv output. Defaults to "csv"}
}
\value{
prints out text with p values, expected vs observed, or csv representation of entire results
}
\description{
Run chi square test of independence on survey results
Updated based on stats consulting
}
