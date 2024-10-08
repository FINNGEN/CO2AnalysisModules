% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_timeCodeWAS.R
\name{execute_timeCodeWAS}
\alias{execute_timeCodeWAS}
\title{execute_timeCodeWAS}
\usage{
execute_timeCodeWAS(exportFolder, cohortTableHandler, analysisSettings)
}
\arguments{
\item{exportFolder}{A string representing the path to the folder where the results will be exported.}

\item{cohortTableHandler}{An R6 object of class \code{CohortTableHandler} containing information about the cohort tables.}

\item{analysisSettings}{A list containing analysis settings, including \code{cohortIds} and \code{minCellCount}.}
}
\value{
A string representing the path to the exported results database.
}
\description{
This function calculates cohort overlaps based on the provided cohort table and analysis settings, and exports the results to a DuckDB database.
}
