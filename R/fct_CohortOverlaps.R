
#' @title execute_CohortOverlaps
#' @description This function calculates cohort overlaps based on the provided cohort table and analysis settings, and exports the results to a DuckDB database.
#'
#' @param exportFolder A string representing the path to the folder where the results will be exported.
#' @param cohortTableHandler An R6 object of class `CohortTableHandler` containing information about the cohort tables.
#' @param analysisSettings A list containing analysis settings, including `cohortIds` and `minCellCount`.
#'
#' @return A string representing the path to the exported results database.
#'
#' @importFrom checkmate assertDirectoryExists assertR6 assertList assertSubset assertNumeric checkFileExists
#' @importFrom ParallelLogger logInfo
#' @importFrom dplyr filter mutate select as_tibble
#' @importFrom HadesExtras removeCohortIdsFromCohortOverlapsTable
#' @importFrom duckdb dbConnect dbDisconnect dbWriteTable dbListTables
#' @importFrom DBI dbGetQuery
#' @importFrom tibble tibble
#' @importFrom yaml as.yaml
#'
#' @export
#'
execute_CohortOverlaps <- function(
    exportFolder,
    cohortTableHandler,
    analysisSettings
) {
  #
  # Check parameters
  #

  exportFolder |> checkmate::assertDirectoryExists()
  cohortTableHandler |> checkmate::assertR6(class = "CohortTableHandler")
  analysisSettings |> assertAnalysisSettings_CohortOverlaps()


  # get parameters from cohortTableHandler
  connection <- cohortTableHandler$connectionHandler$getConnection()
  cohortTable <- cohortTableHandler$cohortTableNames$cohortTable
  cdmDatabaseSchema <- cohortTableHandler$cdmDatabaseSchema
  cohortDatabaseSchema <- cohortTableHandler$cohortDatabaseSchema
  vocabularyDatabaseSchema <- cohortTableHandler$vocabularyDatabaseSchema
  cohortDefinitionSet <- cohortTableHandler$cohortDefinitionSet
  databaseId <- cohortTableHandler$databaseName
  databaseName <- cohortTableHandler$CDMInfo$cdm_source_abbreviation
  databaseDescription <- cohortTableHandler$CDMInfo$cdm_source_name
  vocabularyVersionCdm <- cohortTableHandler$CDMInfo$cdm_version
  vocabularyVersion <- cohortTableHandler$vocabularyInfo$vocabulary_version

  # get parameters from analysisSettings
  cohortIds <- analysisSettings$cohortIds
  minCellCount <- analysisSettings$minCellCount


  #
  # function
  #
  ParallelLogger::logInfo("Calculating cohort overlaps")
  startAnalysisTime <- Sys.time()

  cohortIdsToRemove <- cohortTableHandler$getCohortCounts()$cohortId |>
    setdiff(cohortIds)

  cohortOverlaps <- cohortTableHandler$getCohortsOverlap() |>
    HadesExtras::removeCohortIdsFromCohortOverlapsTable(cohortIdsToRemove)  |>
    dplyr::filter(numberOfSubjects >= minCellCount)

  analysisDuration <- Sys.time() - startAnalysisTime

  #
  # Export
  #
  ParallelLogger::logInfo("Exporting results")
  startExportTime <- Sys.time()

  pathToResultsDatabase <- file.path(exportFolder, "analysisResults.duckdb")
  connection <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  # Database metadata ---------------------------------------------
  database  <- tibble::tibble(
    databaseId = databaseId,
    databaseName =  databaseName,
    databaseDescription =  databaseDescription,
    vocabularyVersionCdm = vocabularyVersionCdm,
    vocabularyVersion = vocabularyVersion
  )
  duckdb::dbWriteTable(connection, "database", database, overwrite = TRUE)

  # Cohort data ------------------------------------------------
  cohortDefinitionSet  <- cohortDefinitionSet |>
    dplyr::mutate(
     cohortId = as.integer(cohortId),
     cohortName = as.character(cohortName),
     shortName = as.character(shortName),
     sql = as.character(sql),
     json = as.character(json),
     subsetParent = as.integer(subsetParent),
     isSubset = as.logical(isSubset),
     subsetDefinitionId = as.integer(subsetDefinitionId)
    )
  duckdb::dbWriteTable(connection, "cohortDefinitionSet",cohortDefinitionSet, overwrite = TRUE)

  # cohort counts ------------------------------------------------
  cohortCounts <- cohortTableHandler$getCohortCounts() |>
    dplyr::mutate(
      cohortId = as.integer(cohortId),
      cohortName = as.character(cohortName),
      cohortEntries = as.integer(cohortEntries),
      cohortSubjects = as.integer(cohortSubjects)
    )
  duckdb::dbWriteTable(connection, "cohortCounts", cohortCounts, overwrite = TRUE)

  # CohortOverlapsCounts ------------------------------------------------
  cohortOverlaps  <- cohortOverlaps |>
    dplyr::mutate(
      cohortIdCombinations = as.character(cohortIdCombinations),
      numberOfSubjects = as.integer(numberOfSubjects)
    )
  duckdb::dbWriteTable(connection, "cohortOverlaps", cohortOverlaps, overwrite = TRUE)

  # analysisInfo ------------------------------------------------
  exportDuration <- Sys.time() - startExportTime

  analysisInfo <- tibble::tibble(
    analysisType = "cohortOverlaps",
    version = "1.0.0",
    analysisSettings = yaml::as.yaml(analysisSettings),
    analysisDuration = analysisDuration,
    exportDuration = exportDuration
  )
  duckdb::dbWriteTable(connection, "analysisInfo", analysisInfo, overwrite = TRUE)

  # close connection
  duckdb::dbDisconnect(connection)

  ParallelLogger::logInfo("Results exported")

  return(pathToResultsDatabase)

}

#' @title Assert Analysis Settings for Cohort Overlaps
#' @description This function checks that the `analysisSettings` list contains the required elements and that they are of the correct types.
#'
#' @param analysisSettings A list containing analysis settings, including `cohortIds` and `minCellCount`.
#'
#' @return TRUE if the settings are valid; otherwise, an error is thrown.
#'
#' @importFrom checkmate assertList assertSubset assertNumeric
#'
#' @export
assertAnalysisSettings_CohortOverlaps <- function(analysisSettings) {
  analysisSettings |> checkmate::assertList()
  analysisSettings |> names()  |> checkmate::assertSubset(c('minCellCount', 'cohortIds'))
  analysisSettings$cohortIds |> checkmate::assertNumeric()
  analysisSettings$minCellCount |> checkmate::assertNumeric(lower = 0)
}


#' @title Check Cohort Overlaps Results
#' @description This function checks the integrity and correctness of the exported cohort overlaps results in a DuckDB database.
#'
#' @param pathToResultsDatabase A string representing the path to the DuckDB database file containing the results.
#'
#' @return TRUE if the results are valid; otherwise, a vector of error messages.
#'
#' @importFrom checkmate checkFileExists checkSubset
#' @importFrom duckdb dbConnect dbDisconnect dbListTables
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr select as_tibble
#'
#' @export
checkResults_CohortOverlaps <- function(pathToResultsDatabase) {

  #
  # Check parameters
  #
  check <- checkmate::checkFileExists(pathToResultsDatabase, extension = 'duckdb')
  if (!check) { errors <- c(errors, check) ; return(errors) }

  #
  # Checking rules
  #
  expected_schemas  <- list(
    database = tibble::tibble(
      name = c("databaseId", "databaseName", "databaseDescription", "vocabularyVersionCdm", "vocabularyVersion"),
      type = c("VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR")
    ),
    cohortDefinitionSet = tibble::tibble(
      name = c("cohortId", "cohortName", "shortName", "sql", "json", "subsetParent", "isSubset", "subsetDefinitionId"),
      type = c("INTEGER", "VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR", "INTEGER", "BOOLEAN", "INTEGER")
    ),
    cohortCounts = tibble::tibble(
      name = c("cohortId", "cohortName", "cohortEntries", "cohortSubjects"),
      type = c("INTEGER", "VARCHAR", "INTEGER", "INTEGER")
    ),
    cohortOverlaps = tibble::tibble(
      name = c("cohortIdCombinations", "numberOfSubjects"),
      type = c("VARCHAR", "INTEGER")
    ),
    analysisInfo = tibble::tibble(
      name = c("analysisType", "version", "analysisSettings", "analysisDuration", "exportDuration"),
      type = c("VARCHAR", "VARCHAR", "VARCHAR", "INTERVAL", "INTERVAL")
    )
  )


  #
  # Check parameters
  #
  errors <- c()

  connection <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  check <- duckdb::dbListTables(connection) |>
    checkmate::checkSubset(c('database', 'cohortDefinitionSet', 'cohortCounts', 'cohortOverlaps', 'analysisInfo'))
  if (!check) { errors <- c(errors, check) ; return(errors) }

  # check schemas
  for (expected_schema_name in  names(expected_schemas)) {
    expected_schema <- expected_schemas[[expected_schema_name]]
    schema <- DBI::dbGetQuery(connection, paste0("PRAGMA table_info('",expected_schema_name, "')")) |>
      dplyr::select(name, type) |>
      dplyr::as_tibble()
    check <- .checkSchema(schema, expected_schema)
    if (check != TRUE) { errors <- c(errors, check) }
  }

  duckdb::dbDisconnect(connection)

  if (length(errors) == 0) { return(TRUE) }
  return(errors)
}

















