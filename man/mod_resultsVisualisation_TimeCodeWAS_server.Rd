% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_resultsVisualisation_TimeCodeWAS.R
\name{mod_resultsVisualisation_TimeCodeWAS_server}
\alias{mod_resultsVisualisation_TimeCodeWAS_server}
\title{Cohort Demographics Visualization Server}
\usage{
mod_resultsVisualisation_TimeCodeWAS_server(id, analysisResults)
}
\arguments{
\item{id}{A string representing the module's namespace.}

\item{analysisResults}{Pooled connection to the analysis results duckdb.}
}
\value{
The module returns server-side logic to generate and manage the cohort overlaps UpSet plot.
}
\description{
Server module for handling the logic of the cohort overlaps visualization UI. This module creates an UpSet plot based on the analysis results and allows the plot and data to be downloaded.
}
