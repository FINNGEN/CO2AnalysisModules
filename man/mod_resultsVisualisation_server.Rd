% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_resultsVisualisation.R
\name{mod_resultsVisualisation_server}
\alias{mod_resultsVisualisation_server}
\title{Results Visualization Dashboard Server}
\usage{
mod_resultsVisualisation_server(
  id,
  resultsVisualisationModuleServer,
  analysisResults
)
}
\arguments{
\item{id}{A string representing the module's namespace.}

\item{resultsVisualisationModuleServer}{A server function representing the specific visualization module's server logic.}

\item{analysisResults}{Pooled connection to the analysis results duckdb.}
}
\value{
The module returns server-side logic to manage the results visualization dashboard.
}
\description{
Server module for handling the logic of the results visualization dashboard.
This includes rendering the cohort definitions table and invoking the specific visualization module's server logic.
}
