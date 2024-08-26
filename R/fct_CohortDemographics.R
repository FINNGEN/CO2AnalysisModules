
#' @title execute_CohortDemographics
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
#' @importFrom HadesExtras removeCohortIdsFromCohortDemographicsTable
#' @importFrom duckdb dbConnect dbDisconnect dbWriteTable dbListTables
#' @importFrom DBI dbGetQuery
#' @importFrom tibble tibble
#' @importFrom yaml as.yaml
#'
#' @export
#'
execute_CohortDemographics <- function(
    exportFolder,
    cohortTableHandler,
    analysisSettings
) {
  #
  # Check parameters
  #
  groups <- c("calendarYear", "ageGroup", "gender")
  validReferenceYears <- c("cohort_start_date", "cohort_end_date", "birth_datetime")

  exportFolder |> checkmate::assertDirectoryExists()
  cohortTableHandler |> checkmate::assertR6(class = "CohortTableHandler")
  analysisSettings |> assertAnalysisSettings_CohortDemographics()


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
  referenceYears <- analysisSettings$referenceYears
  groupBy <- analysisSettings$groupBy
  minCellCount <- analysisSettings$minCellCount


  #
  # function
  #
  ParallelLogger::logInfo("Calculating cohort demographics")
  startAnalysisTime <- Sys.time()

  demographicsCounts  <- HadesExtras::getCohortDemographicsCounts(
    connection = connection,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cohortIds = cohortIds,
    referenceYears = referenceYears
  )

  if (length(groupBy) < length(groups)) {
    demographicsCounts <- demographicsCounts |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("cohortId", groupBy)))) |>
      dplyr::summarize(count = sum(count)) |>
      dplyr::ungroup()

    demographicsCounts <- demographicsCounts |>
      dplyr::bind_cols(
        tibble::tibble(
          calendarYear = NA_real_,
          ageGroup = NA_character_,
          gender = NA_character_
        ) |>
          dplyr::select(dplyr::all_of(setdiff(groups, groupBy)))
      )
  }

  if (minCellCount > 0) {
    demographicsCounts <- demographicsCounts |>
      dplyr::filter(count >= minCellCount)
  }

  analysisDuration <- Sys.time() - startAnalysisTime

  #
  # Export
  #
  ParallelLogger::logInfo("Exporting results")
  startExportTime <- Sys.time()

  pathToResultsDatabase <- file.path(exportFolder, "analysisResults.duckdb")
  connection <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  # Database metadata ---------------------------------------------
  databaseInfo  <- tibble::tibble(
    databaseId = databaseId,
    databaseName =  databaseName,
    databaseDescription =  databaseDescription,
    vocabularyVersionCdm = vocabularyVersionCdm,
    vocabularyVersion = vocabularyVersion
  )
  duckdb::dbWriteTable(connection, "databaseInfo", databaseInfo, overwrite = TRUE)

  # Cohort data ------------------------------------------------
  cohortsInfo  <- cohortDefinitionSet |>
    dplyr::mutate(
     cohortId = as.integer(cohortId),
     cohortName = as.character(cohortName),
     shortName = as.character(shortName),
     sql = as.character(sql),
     json = as.character(json),
     subsetParent = as.integer(subsetParent),
     isSubset = as.logical(isSubset),
     subsetDefinitionId = as.integer(subsetDefinitionId)
    ) |>
    dplyr::left_join(
      cohortTableHandler$getCohortCounts() |>
        dplyr::transmute(
          cohortId = as.integer(cohortId),
          cohortEntries = as.integer(cohortEntries),
          cohortSubjects = as.integer(cohortSubjects)
        ),
      by = "cohortId"
    ) |>
    dplyr::mutate(use = ifelse(cohortId == {cohortIds}, "Selected", ""))

  duckdb::dbWriteTable(connection, "cohortsInfo",cohortsInfo, overwrite = TRUE)

  # CohortDemographicsCounts ------------------------------------------------
  demographicsCounts  <- demographicsCounts |>
    dplyr::transmute(
      databaseId = as.character(databaseId),
      cohortId = as.integer(cohortId),
      referenceYear = as.character(referenceYear),
      calendarYear = as.integer(calendarYear),
      ageGroup = as.character(ageGroup),
      gender = as.character(gender),
      count = as.integer(count)
    )
  duckdb::dbWriteTable(connection, "demographicsCounts", demographicsCounts, overwrite = TRUE)

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

#' @title Assert Analysis Settings for Cohort Demographics Overlaps
#' @description Validates the `analysisSettings` list to ensure it contains the required elements (`cohortIds`, `referenceYears`, `groupBy`, `minCellCount`) with correct types and values. This function is specifically designed for checking settings related to cohort demographics analysis.
#'
#' @param analysisSettings A list containing analysis settings. It must include the following elements:
#' \describe{
#'   \item{cohortIds}{A numeric vector of cohort IDs.}
#'   \item{referenceYears}{A character vector specifying the reference years, must be one of `cohort_start_date`, `cohort_end_date`, or `birth_datetime`.}
#'   \item{groupBy}{A character vector indicating the demographic groups for analysis, must be one of `calendarYear`, `ageGroup`, or `gender`.}
#'   \item{minCellCount}{A numeric value representing the minimum cell count, must be 0 or higher.}
#' }
#'
#' @return Returns `TRUE` if all settings are valid; otherwise, throws an error.
#'
#' @importFrom checkmate assertList assertSubset assertNumeric assertCharacter
#'
#' @export
assertAnalysisSettings_CohortDemographics <- function(analysisSettings) {
  groups <- c("calendarYear", "ageGroup", "gender")
  validReferenceYears <- c("cohort_start_date", "cohort_end_date", "birth_datetime")

  analysisSettings |> checkmate::assertList()
  analysisSettings |> names()  |> checkmate::assertSubset(c('cohortIds', 'referenceYears', 'groupBy', 'minCellCount'))
  analysisSettings$cohortIds |> checkmate::assertNumeric()
  analysisSettings$referenceYears |> checkmate::assertCharacter(min.len = 1)
  analysisSettings$referenceYears |> checkmate::assertSubset(validReferenceYears)
  analysisSettings$groupBy |> checkmate::assertCharacter(min.len = 1)
  analysisSettings$groupBy |> checkmate::assertSubset(groups)
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
checkResults_CohortDemographics <- function(pathToResultsDatabase) {

  #
  # Checking rules
  #
  expectedSchemas  <- list(
    databaseInfo = tibble::tibble(
      name = c("databaseId", "databaseName", "databaseDescription", "vocabularyVersionCdm", "vocabularyVersion"),
      type = c("VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR")
    ),
    cohortsInfo = tibble::tibble(
      name = c("cohortId", "cohortName", "shortName", "sql", "json", "subsetParent", "isSubset", "subsetDefinitionId", "cohortEntries", "cohortSubjects", "use"),
      type = c("INTEGER", "VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR", "INTEGER", "BOOLEAN", "INTEGER", "INTEGER", "INTEGER", "VARCHAR")
    ),
    demographicsCounts = tibble::tibble(
      name = c("databaseId", "cohortId", "referenceYear", "calendarYear", "ageGroup", "gender", "count"),
      type = c("VARCHAR", "INTEGER", "VARCHAR", "INTEGER", "VARCHAR", "VARCHAR", "INTEGER")
    ),
    analysisInfo = tibble::tibble(
      name = c("analysisType", "version", "analysisSettings", "analysisDuration", "exportDuration"),
      type = c("VARCHAR", "VARCHAR", "VARCHAR", "INTERVAL", "INTERVAL")
    )
  )

  #
  # Check
  #
  errors  <- .checkDatabase(pathToResultsDatabase, expectedSchemas)
  return(errors)
}



