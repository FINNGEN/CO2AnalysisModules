% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_utils.R
\name{.checkSchema}
\alias{.checkSchema}
\title{Check Schema Consistency}
\usage{
.checkSchema(schema, expectedSchema)
}
\arguments{
\item{schema}{A tibble representing the schema of a database table, with columns \code{name} and \code{type}.}

\item{expectedSchema}{A tibble representing the expected schema of the database table, with columns \code{name} and \code{type}.}
}
\value{
Returns \code{TRUE} if the schema matches the expected schema. Otherwise, it returns an error message indicating the discrepancies.
}
\description{
This function compares a given schema against an expected schema and checks for discrepancies.
}
