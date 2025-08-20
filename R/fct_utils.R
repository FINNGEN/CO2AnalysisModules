
#' @title Check Database Schema
#' @description This function checks the schema of a DuckDB database against expected schemas.
#'
#' @param pathToResultsDatabase A string representing the path to the DuckDB database file.
#' @param expectedSchemas A named list of tibbles, where each tibble represents the expected schema of a database table with columns `name` and `type`.
#'
#' @return Returns `TRUE` if all scheme match the expected scheme. Otherwise, it returns a list of error messages indicating the discrepancies.
#'
#' @importFrom checkmate checkFileExists checkSubset
#' @importFrom duckdb dbConnect dbListTables dbDisconnect
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr select as_tibble
#' @importFrom purrr map
#' @importFrom utils capture.output data write.csv
#'
.checkDatabase <- function(pathToResultsDatabase, expectedSchemas) {

  #
  # Check parameters
  #
  check <- checkmate::checkFileExists(pathToResultsDatabase, extension = 'duckdb')
  if (!check) { errors <- c(errors, check) ; return(errors) }

  #
  # Function
  #
  errors <- c()

  connection <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  check <- duckdb::dbListTables(connection) |>
    checkmate::checkSubset(expectedSchemas |> names())
  if (!check) { errors <- c(errors, check) ; return(errors) }

  # check scheme
  for (expectedSchemaName in  names(expectedSchemas)) {
    expectedSchema <- expectedSchemas[[expectedSchemaName]]
    schema <- DBI::dbGetQuery(connection, paste0("PRAGMA table_info('",expectedSchemaName, "')")) |>
      dplyr::select(name, type) |>
      dplyr::as_tibble()
    check <- .checkSchema(schema, expectedSchema)
    if (check != TRUE) { errors <- c(errors, check) }
  }

  duckdb::dbDisconnect(connection)

  if (length(errors) == 0) { return(TRUE) }
  return(errors)
}

#' @title Check Schema Consistency
#' @description This function compares a given schema against an expected schema and checks for discrepancies.
#'
#' @param schema A tibble representing the schema of a database table, with columns `name` and `type`.
#' @param expectedSchema A tibble representing the expected schema of the database table, with columns `name` and `type`.
#'
#' @return Returns `TRUE` if the schema matches the expected schema. Otherwise, it returns an error message indicating the discrepancies.
#'
#' @importFrom checkmate assertTibble
#' @importFrom dplyr anti_join
#'
.checkSchema  <- function(schema, expectedSchema) {
  schema  |> checkmate::assertTibble(types = c('character', 'character'))
  expectedSchema  |> checkmate::assertTibble(types = c('character', 'character'))

  errors_schema <- dplyr::anti_join(schema, expectedSchema, by = c('name', 'type'))

  if (nrow(errors_schema) == 0){ return(TRUE) }

  error_message <- paste(
    'Schema does not match expected schema:\n',
    'Got errors:\n\t', errors_schema |> capture.output() |> (\(x) x[-c(1, 3)])() |> paste(collapse = '\n\t'),
    '\nExpected schema:\n\t', expectedSchema |> capture.output() |> (\(x) x[-c(1, 3)])() |> paste(collapse = '\n\t'),
    '\n'
  )

  return(error_message)
}



# .numericRangeFilter: internal JS function to filter numeric values in reactable columns.
# Supports ranges (e.g., -2--0.5, 1-4), comparison operators (>=, <=, >, <, ==), and bare numbers.

.numericRangeFilter <- htmlwidgets::JS(
  "function(rows, columnId, filterValue) {
         if (!filterValue) return rows;

         // Normalize: trim + remove all spaces (so '-2 - -0.5' -> '-2--0.5')
         const val = filterValue.trim().replace(/\\s+/g, '');
         if (!val) return rows;

         const EPS = 1e-9;

         // Number pattern: optional '-', decimals, optional scientific notation
         const NUM = '-?\\\\d+(?:\\\\.\\\\d+)?(?:[eE][+-]?\\\\d+)?';

         const reRange = new RegExp(`^(${NUM})-(${NUM})$`);
         const reGte   = new RegExp(`^>=(${NUM})$`);
         const reLte   = new RegExp(`^<=(${NUM})$`);
         const reGt    = new RegExp(`^>(${NUM})$`);
         const reLt    = new RegExp(`^<(${NUM})$`);
         const reEq    = new RegExp(`^==(${NUM})$`);
         const reNum   = new RegExp(`^${NUM}$`);

         const toNum = (x) => {
           const n = parseFloat(x);
           return Number.isNaN(n) ? null : n;
         };

         // Range: e.g., 5-10, -2--0.5
         let m = val.match(reRange);
         if (m) {
           let min = toNum(m[1]);
           let max = toNum(m[2]);
           if (min == null || max == null) return rows;
           if (min > max) { const t = min; min = max; max = t; } // tolerate reversed ranges
           return rows.filter(row => {
             const v = Number(row.values[columnId]);
             return v != null && !Number.isNaN(v) && (v > min - EPS) && (v < max + EPS);
           });
         }

         // >=X (supports negatives)
         m = val.match(reGte);
         if (m) {
           const num = toNum(m[1]);
           return rows.filter(row => {
             const v = Number(row.values[columnId]);
             return v != null && !Number.isNaN(v) && (v > num || Math.abs(v - num) < EPS);
           });
         }

         // <=X
         m = val.match(reLte);
         if (m) {
           const num = toNum(m[1]);
           return rows.filter(row => {
             const v = Number(row.values[columnId]);
             return v != null && !Number.isNaN(v) && (v < num || Math.abs(v - num) < EPS);
           });
         }

         // >X
         m = val.match(reGt);
         if (m) {
           const num = toNum(m[1]);
           return rows.filter(row => {
             const v = Number(row.values[columnId]);
             return v != null && !Number.isNaN(v) && v > num + EPS;
           });
         }

         // <X
         m = val.match(reLt);
         if (m) {
           const num = toNum(m[1]);
           return rows.filter(row => {
             const v = Number(row.values[columnId]);
             return v != null && !Number.isNaN(v) && v < num - EPS;
           });
         }

         // ==X
         m = val.match(reEq);
         if (m) {
           const num = toNum(m[1]);
           return rows.filter(row => {
             const v = Number(row.values[columnId]);
             return v != null && !Number.isNaN(v) && Math.abs(v - num) < EPS;
           });
         }

         // Bare number (no operator): treat as equality with tolerance
         if (reNum.test(val)) {
           const num = toNum(val);
           return rows.filter(row => {
             const v = Number(row.values[columnId]);
             return v != null && !Number.isNaN(v) && Math.abs(v - num) < EPS;
           });
         }

         // If input doesn't match any pattern, don't filter
         return rows;
       }"
)

