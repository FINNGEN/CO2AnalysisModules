
#' @title Check Schema Consistency
#' @description This function compares a given schema against an expected schema and checks for discrepancies.
#'
#' @param schema A tibble representing the schema of a database table, with columns `name` and `type`.
#' @param expected_schema A tibble representing the expected schema of the database table, with columns `name` and `type`.
#'
#' @return Returns `TRUE` if the schema matches the expected schema. Otherwise, it returns an error message indicating the discrepancies.
#'
#' @importFrom checkmate assertTibble
#' @importFrom dplyr anti_join
#'
.checkSchema  <- function(schema, expected_schema) {
  schema  |> checkmate::assertTibble(types = c('character', 'character'))
  expected_schema  |> checkmate::assertTibble(types = c('character', 'character'))

  errors_schema <- dplyr::anti_join(schema, expected_schema, by = c('name', 'type'))

  if (nrow(errors_schema) == 0){ return(TRUE) }

  error_message <- paste(
    'Schema does not match expected schema:\n',
    'Got errors:\n\t', errors_schema |> capture.output() |> (\(x) x[-c(1, 3)])() |> paste(collapse = '\n\t'),
    '\nExpected schema:\n\t', expected_schema |> capture.output() |> (\(x) x[-c(1, 3)])() |> paste(collapse = '\n\t'),
    '\n'
  )

  return(error_message)
}
