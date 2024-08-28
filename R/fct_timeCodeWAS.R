
#' @title execute_timeCodeWAS
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
#' @importFrom HadesExtras removeCohortIdsFromtimeCodeWASTable
#' @importFrom duckdb dbConnect dbDisconnect dbWriteTable dbListTables
#' @importFrom DBI dbGetQuery
#' @importFrom tibble tibble
#' @importFrom yaml as.yaml
#'
#' @export
#'
execute_timeCodeWAS <- function(
    exportFolder,
    cohortTableHandler,
    analysisSettings
) {
  #
  # Check parameters
  #
  exportFolder |> checkmate::assertDirectoryExists()
  cohortTableHandler |> checkmate::assertR6(class = "CohortTableHandler")
  analysisSettings <- analysisSettings |> assertAnalysisSettings_timeCodeWAS()


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
  cohortIdCases <- analysisSettings$cohortIdCases
  cohortIdControls <- analysisSettings$cohortIdControls
  analysisIds <- analysisSettings$analysisIds
  temporalStartDays <- analysisSettings$temporalStartDays
  temporalEndDays <- analysisSettings$temporalEndDays
  chunksSizeNOutcomes <- analysisSettings$chunksSizeNOutcomes


  #
  # function
  #
  ParallelLogger::logInfo("Calculating timeCodeWAS")
  startAnalysisTime <- Sys.time()

  covariateSettings  <- HadesExtras::FeatureExtraction_createTemporalCovariateSettingsFromList(
    analysisIds = analysisIds,
    temporalStartDays = temporalStartDays,
    temporalEndDays = temporalEndDays
  )

  ParallelLogger::logInfo("Getting FeatureExtraction for cases and controls")
  covariateCasesControls <- FeatureExtraction::getDbCovariateData(
    connection = connection,
    cohortTable = cohortTable,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    covariateSettings = covariateSettings,
    cohortIds = c(cohortIdCases, cohortIdControls),
    aggregated = T
  )

  ParallelLogger::logInfo("calcualting number of subjects with observation case and controls in each time window")

  cohortTbl <- dplyr::tbl(connection, HadesExtras::tmp_inDatabaseSchema(cohortDatabaseSchema, cohortTable))
  observationPeriodTbl <- dplyr::tbl(connection,  HadesExtras::tmp_inDatabaseSchema(cdmDatabaseSchema, "observation_period"))

  # TEMP, when therea are more than one covariate settings, get the first (it should use timeWindow from server instead)
  # if covariateSettings doent have the temporalStartDays, it means it is a list of settings
  if (length(covariateSettings$temporalStartDays) == 0) {
    covariateSettings <- covariateSettings[[1]]
  }

  timeWindows <- tibble::tibble(
    id_window =  as.integer(1:length(covariateSettings$temporalStartDays)),
    start_day = as.integer(covariateSettings$temporalStartDays),
    end_day = as.integer(covariateSettings$temporalEndDays)
  )
  timeWindowsTbl <- HadesExtras::tmp_dplyr_copy_to(connection, timeWindows, overwrite = TRUE)

  windowCounts <- cohortTbl |>
    dplyr::filter(cohort_definition_id %in% c(cohortIdCases, cohortIdControls)) |>
    dplyr::left_join(
      # at the moment take the first and last
      observationPeriodTbl |>
        dplyr::select(person_id, observation_period_start_date, observation_period_end_date) |>
        dplyr::group_by(person_id) |>
        dplyr::summarise(
          observation_period_start_date = min(observation_period_start_date, na.rm = TRUE),
          observation_period_end_date = max(observation_period_end_date, na.rm = TRUE)
        ),
      by = c("subject_id" = "person_id")) |>
    dplyr::cross_join(timeWindowsTbl) |>
    dplyr::filter(
      # exclude if window is under the observation_period_start_date or over the observation_period_end_date
      !(dateAdd("day", end_day, cohort_start_date) < observation_period_start_date |
          dateAdd("day", start_day, cohort_start_date) > observation_period_end_date )
    ) |>
    dplyr::group_by(cohort_definition_id, id_window) |>
    dplyr::count() |>
    dplyr::collect()

  windowCounts <-  windowCounts |>
    dplyr::mutate(cohort_definition_id = dplyr::case_when(
      cohort_definition_id == cohortIdCases ~ "n_cases",
      cohort_definition_id == cohortIdControls ~ "n_controls",
      TRUE ~ as.character(NA)
    )) |>
    tidyr::spread(key = cohort_definition_id, n)|>
    dplyr::rename(timeId = id_window)


  ParallelLogger::logInfo("Calculating time counts")

  timeCodeWASResults <-
    dplyr::full_join(
      covariateCasesControls$covariates |> tibble::as_tibble() |>
        dplyr::filter(cohortDefinitionId == cohortIdCases) |>
        dplyr::select(covariateId, timeId, n_cases_yes=sumValue),
      covariateCasesControls$covariates |> tibble::as_tibble() |>
        dplyr::filter(cohortDefinitionId == cohortIdControls) |>
        dplyr::select( covariateId, timeId, n_controls_yes=sumValue),
      by = c("covariateId", "timeId")
    )  |>
    dplyr::left_join(
      windowCounts,
      by="timeId"
    ) |>
    dplyr::transmute(
      covariateId = covariateId,
      timeId = timeId,
      nCasesYes = dplyr::if_else(is.na(n_cases_yes), 0, n_cases_yes),
      nControlsYes = dplyr::if_else(is.na(n_controls_yes), 0, n_controls_yes),
      nCases = dplyr::if_else(is.na(n_cases), 0, n_cases),
      nControls = dplyr::if_else(is.na(n_controls), 0, n_controls),
      # force n_cases to be same or lower than n_cases_yes, same for controls
      nCases = dplyr::if_else(nCases < nCasesYes, nCasesYes, nCases),
      nControls = dplyr::if_else(nControls < nControlsYes, nControlsYes, nControls),
      #
      nCasesNo = nCases - nCasesYes,
      nControlsNo = nControls - nControlsYes
    )

  ParallelLogger::logInfo("Adding Fisher test output")
  timeCodeWASResults <- .addTestToCodeCounts(timeCodeWASResults)

  analysisRef  <-  covariateCasesControls$analysisRef  |> dplyr::collect()

  covariateRef  <- covariateCasesControls$covariateRef  |> dplyr::collect()

  ParallelLogger::logInfo("CohortDiagnostics_runTimeCodeWAS completed")



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
    dplyr::mutate(
      use = dplyr::case_when(
        cohortId == cohortIdCases ~ "cases",
        cohortId == cohortIdControls ~ "controls",
        TRUE ~ ""
      )
    )

  duckdb::dbWriteTable(connection, "cohortsInfo",cohortsInfo, overwrite = TRUE)

  # timeCodeWASCounts ------------------------------------------------
  timeCodeWASResults  <-  timeCodeWASResults |>
    dplyr::transmute(
      databaseId = as.character({{databaseId}}),
      covariateId = as.integer(covariateId),
      timeId = as.character(timeId),
      nCasesYes = as.integer(nCasesYes),
      nControlsYes = as.integer(nControlsYes),
      nCasesNo = as.integer(nCasesNo),
      nControlsNo = as.integer(nControlsNo),
      pValue = as.double(countsPValue),
      oddsRatio = as.double(countsOddsRatio),
      modelType = as.character(countsTest)
    )
  duckdb::dbWriteTable(connection, "timeCodeWASResults", timeCodeWASResults, overwrite = TRUE)

  analysisRef <- analysisRef |>
    dplyr::transmute(
      analysisId = as.integer(analysisId),
      analysisName = as.character(analysisName),
      domainId = as.character(domainId),
      isBinary = as.character(isBinary),
      missingMeansZero = as.character(missingMeansZero)
    )
  duckdb::dbWriteTable(connection, "analysisRef", analysisRef, overwrite = TRUE)

  covariateRef <- covariateRef |>
    dplyr::transmute(
      covariateId = as.integer(covariateId),
      covariateName = as.character(covariateName),
      analysisId = as.integer(analysisId),
      conceptId = as.integer(conceptId),
      valueAsConceptId = as.integer(valueAsConceptId),
      collisions = as.character(collisions)
    )
  duckdb::dbWriteTable(connection, "covariateRef", covariateRef, overwrite = TRUE)

  timeRef <- timeWindows |>
    dplyr::transmute(
      timeId = as.integer(id_window),
      startDay = as.integer(start_day),
      endDay = as.integer(end_day)
    )
  duckdb::dbWriteTable(connection, "timeRef", timeRef, overwrite = TRUE)

  # analysisInfo ------------------------------------------------
  exportDuration <- Sys.time() - startExportTime

  analysisInfo <- tibble::tibble(
    analysisType = "timeCodeWAS",
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
assertAnalysisSettings_timeCodeWAS <- function(analysisSettings) {

  analysisSettings |> checkmate::assertList()
  c('cohortIdCases', 'cohortIdControls', 'analysisIds', 'temporalStartDays', 'temporalEndDays')  |> checkmate::assertSubset(names(analysisSettings))
  analysisSettings$cohortIdCases |> checkmate::assertNumeric()
  analysisSettings$cohortIdControls |> checkmate::assertNumeric()
  analysisSettings$analysisIds |> checkmate::assertNumeric()
  analysisSettings$temporalStartDays |> checkmate::assertNumeric()
  analysisSettings$temporalEndDays |> checkmate::assertNumeric()

  if(is.null(analysisSettings$minCellCount)){
    analysisSettings$minCellCount <- 1
  }
  analysisSettings$minCellCount |> checkmate::assertNumeric()


  return(analysisSettings)

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
checkResults_timeCodeWAS <- function(pathToResultsDatabase) {

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
    timeCodeWASResults = tibble::tibble(
      name = c("databaseId", "covariateId", "timeId", "nCasesYes", "nControlsYes", "nCasesNo", "nControlsNo", "pValue", "oddsRatio", "modelType"),
      type = c("VARCHAR", "INTEGER", "VARCHAR", "INTEGER", "INTEGER", "INTEGER", "INTEGER", "DOUBLE", "DOUBLE", "VARCHAR")
    ),
    analysisRef = tibble::tibble(
      name = c("analysisId", "analysisName", "domainId", "isBinary", "missingMeansZero"),
      type = c("INTEGER", "VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR")
    ),
    covariateRef = tibble::tibble(
      name = c("covariateId", "covariateName", "analysisId", "conceptId", "valueAsConceptId", "collisions"),
      type = c("INTEGER", "VARCHAR", "INTEGER", "INTEGER", "INTEGER", "VARCHAR")
    ),
    analysisInfo = tibble::tibble(
      name = c("analysisType", "version", "analysisSettings", "analysisDuration", "exportDuration"),
      type = c("VARCHAR", "VARCHAR", "VARCHAR", "INTERVAL", "INTERVAL")
    ),
    timeRef = tibble::tibble(
      name = c("timeId", "startDay", "endDay"),
      type = c("INTEGER", "INTEGER", "INTEGER")
    )
  )

  #
  # Check
  #
  errors  <- .checkDatabase(pathToResultsDatabase, expectedSchemas)
  return(errors)
}



