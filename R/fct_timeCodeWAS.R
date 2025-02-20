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
    analysisSettings) {
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


  #
  # if cohortIdControls is 0 create the control cohort first
  #
  if (cohortIdControls == 0) {
    ParallelLogger::logInfo("Creating match control cohort")
    
    cohortDefinitionSet <- cohortTableHandler$cohortDefinitionSet
    newCohortId <- setdiff(1:1000, cohortDefinitionSet$cohortId)[1]
    newCohortName <- paste0("Any patient not in ", cohortDefinitionSet |> dplyr::filter(cohortId == cohortIdCases) |> dplyr::pull(cohortName))
    newCohortShortName <- paste0("ALL\u2229", substr(cohortDefinitionSet |> dplyr::filter(cohortId == cohortIdCases) |> dplyr::pull(shortName), 1, 15))

    ParallelLogger::logInfo("Creating no case cohort")
    cohortDefinitionSetAllMinusCase <- tibble::tibble(
      cohortId = newCohortId,
      cohortName = newCohortName,
      shortName = newCohortShortName,
      json = "{}",
      sql = paste0(
        "-- This code inserts records from an external cohort table into the cohort table of the cohort database schema.
      DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
      INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
      SELECT @target_cohort_id AS cohort_definition_id, op.person_id AS subject_id, op.observation_period_start_date AS cohort_start_date, op.observation_period_end_date AS cohort_end_date
      FROM ", cdmDatabaseSchema, ".observation_period AS op
      WHERE op.person_id NOT IN (
        SELECT subject_id
        FROM @target_database_schema.@target_cohort_table
        WHERE cohort_definition_id = ", cohortIdCases, "
      )
      ")
    )

    cohortDefinitionSet <- dplyr::bind_rows(cohortDefinitionSet, cohortDefinitionSetAllMinusCase)

    ParallelLogger::logInfo("Creating match control cohort")
    # Match to sex and bday, match ratio 10
    subsetDef <- CohortGenerator::createCohortSubsetDefinition(
      name = "",
      definitionId = newCohortId,
      subsetOperators = list(
        HadesExtras::createMatchingSubset(
          matchToCohortId = cohortIdCases,
          matchRatio = 10,
          matchSex = TRUE,
          matchBirthYear = TRUE,
          matchCohortStartDateWithInDuration = FALSE,
          newCohortStartDate = "asMatch",
          newCohortEndDate = "keep"
        )
      )
    )

    cohortDefinitionSet <- cohortDefinitionSet |>
      CohortGenerator::addCohortSubsetDefinition(subsetDef, targetCohortIds = newCohortId)

    cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)

    cohortIdControls <- newCohortId
    cohortDefinitionSet  <- cohortTableHandler$cohortDefinitionSet
  }

  #
  # function
  #
  ParallelLogger::logInfo("Calculating timeCodeWAS")
  startAnalysisTime <- Sys.time()

  covariateSettings <- HadesExtras::FeatureExtraction_createTemporalCovariateSettingsFromList(
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
    aggregated = T, 
    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
  )

  # binary
  timeCovariateCountsBinary <- tibble::tibble()
  if (is.null(covariateCasesControls$covariates) == FALSE &&
    covariateCasesControls$covariates |>
      dplyr::count() |>
      dplyr::pull(n) != 0) {
    ParallelLogger::logInfo("Running statistical test for binary covariates")

    ParallelLogger::logInfo("calcualting number of subjects with observation case and controls in each time window")

    cohortTbl <- dplyr::tbl(connection, dbplyr::in_schema(cohortDatabaseSchema, cohortTable))
    observationPeriodTbl <- dplyr::tbl(connection, dbplyr::in_schema(cdmDatabaseSchema, "observation_period"))

    timeWindows <- tibble::tibble(
      id_window = as.integer(1:length(temporalStartDays)),
      start_day = as.integer(temporalStartDays),
      end_day = as.integer(temporalEndDays)
    )
    timeWindowsTbl <- dplyr::copy_to(connection, timeWindows, overwrite = TRUE)
    
    windowCounts <- dplyr::cross_join(
      timeWindowsTbl,
      cohortTbl |>
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
          by = c("subject_id" = "person_id")
        )
    ) |>
      dplyr::filter(
        # exclude if window is under the observation_period_start_date or over the observation_period_end_date
        dbplyr::sql("NOT(
          DATEADD('day', end_day, cohort_start_date) < observation_period_start_date OR 
          DATEADD('day', start_day, cohort_start_date) > observation_period_end_date
        )")
      ) |>
      dplyr::group_by(cohort_definition_id, id_window) |>
    dplyr::count() |>
      dplyr::collect()

    windowCounts <- windowCounts |>
      dplyr::mutate(cohort_definition_id = dplyr::case_when(
        cohort_definition_id == cohortIdCases ~ "nCasesInWindow",
        cohort_definition_id == cohortIdControls ~ "nControlsInWindow",
        TRUE ~ as.character(NA)
      )) |>
      tidyr::spread(key = cohort_definition_id, n) |>
      dplyr::rename(timeId = id_window)


    ParallelLogger::logInfo("Calculating time counts")

    timeCovariateCountsBinary <-
      dplyr::full_join(
        covariateCasesControls$covariates |> tibble::as_tibble() |>
          dplyr::filter(cohortDefinitionId == cohortIdCases) |>
          dplyr::select(covariateId, timeId, nCasesYes = sumValue),
        covariateCasesControls$covariates |> tibble::as_tibble() |>
          dplyr::filter(cohortDefinitionId == cohortIdControls) |>
          dplyr::select(covariateId, timeId, nControlsYes = sumValue),
        by = c("covariateId", "timeId")
      ) |>
      dplyr::left_join(
        windowCounts,
        by = "timeId"
      ) |>
      dplyr::transmute(
        covariateId = covariateId,
        timeId = timeId,
        nCasesYes = dplyr::if_else(is.na(nCasesYes), 0, nCasesYes),
        nControlsYes = dplyr::if_else(is.na(nControlsYes), 0, nControlsYes),
        nCasesInWindow = dplyr::if_else(is.na(nCasesInWindow), 0, nCasesInWindow),
        nControlsInWindow = dplyr::if_else(is.na(nControlsInWindow), 0, nControlsInWindow),
        # force nCasesInWindow to be same or lower than nCasesYes, same for controls
        nCasesInWindow = dplyr::if_else(nCasesInWindow < nCasesYes, nCasesYes, nCasesInWindow),
        nControlsInWindow = dplyr::if_else(nControlsInWindow < nControlsYes, nControlsYes, nControlsInWindow),
        #
        nCasesNo = nCasesInWindow - nCasesYes,
        nControlsNo = nControlsInWindow - nControlsYes
      )

    ParallelLogger::logInfo("Adding Fisher test output")
    timeCovariateCountsBinary <- .addTestToCodeCounts(timeCovariateCountsBinary) |>
      dplyr::transmute(
        covariateId = covariateId,
        timeId = timeId,
        covariateType = "binary",
        nCasesYes = nCasesYes,
        nCasesInWindow = nCasesInWindow,
        meanCases = nCasesYes / nCasesInWindow,
        sdCases = sqrt(meanCases * (1 - meanCases)),
        nControlsYes = nControlsYes,
        nControlsInWindow = nControlsInWindow,
        meanControls = nControlsYes / nControlsInWindow,
        sdControls = sqrt(meanControls * (1 - meanControls)),
        pValue = countsPValue,
        oddsRatio = dplyr::if_else(is.infinite(countsOddsRatio), .Machine$double.xmax, countsOddsRatio),
        modelType = countsTest,
        runNotes = ""
      )
  }

  # continuous
  timeCovariateCountsContinuous <- tibble::tibble()
  if (is.null(covariateCasesControls$covariatesContinuous) == FALSE &&
    covariateCasesControls$covariatesContinuous |>
      dplyr::count() |>
      dplyr::pull(n) != 0) {
    ParallelLogger::logInfo("Running statistical test for continuous covariates")

    timeCovariateCountsContinuous <- dplyr::full_join(
      covariateCasesControls$covariatesContinuous |>
        dplyr::filter(cohortDefinitionId == {{ cohortIdCases }}) |>
        dplyr::select(covariateId, timeId, nCasesYesWithValue = countValue, meanValueCases = averageValue, sdValueCases = standardDeviation),
      covariateCasesControls$covariatesContinuous |>
        dplyr::filter(cohortDefinitionId == {{ cohortIdControls }}) |>
        dplyr::select(covariateId, timeId, nControlsYesWithValue = countValue, meanValueControls = averageValue, sdValueControls = standardDeviation),
      by = c("covariateId", "timeId")
    ) |>
      dplyr::collect() |>
      dplyr::left_join(
        windowCounts,
        by = "timeId"
      ) |>
      dplyr::mutate(
        nCasesYesWithValue = ifelse(is.na(nCasesYesWithValue), 0, nCasesYesWithValue),
        nControlsYesWithValue = ifelse(is.na(nControlsYesWithValue), 0, nControlsYesWithValue),
        meanValueCases = ifelse(is.na(meanValueCases), 0, meanValueCases),
        nCasesInWindow = dplyr::if_else(is.na(nCasesInWindow), 0, nCasesInWindow),
        nControlsInWindow = dplyr::if_else(is.na(nControlsInWindow), 0, nControlsInWindow),
        # force nCasesInWindow to be same or lower than nCasesYes, same for controls
        nCasesInWindow = dplyr::if_else(nCasesInWindow < nCasesYesWithValue, nCasesYesWithValue, nCasesInWindow),
        nControlsInWindow = dplyr::if_else(nControlsInWindow < nControlsYesWithValue, nControlsYesWithValue, nControlsInWindow)
      )

    timeCovariateCountsContinuous <- .addTestTotibbleWithValueSummary(timeCovariateCountsContinuous) |>
      dplyr::transmute(
        covariateId = covariateId,
        timeId = timeId,
        covariateType = "continuous",
        nCasesYes = nCasesYesWithValue,
        nCasesInWindow = nCasesInWindow,
        meanCases = meanValueCases,
        sdCases = sdValueCases,
        nControlsYes = nControlsYesWithValue,
        nControlsInWindow = nControlsInWindow,
        meanControls = meanValueControls,
        sdControls = sdValueControls,
        pValue = continuousPValue,
        oddsRatio = dplyr::if_else(is.infinite(continuousOddsRatio), .Machine$double.xmax, continuousOddsRatio),
        modelType = continuousTest,
        runNotes = ""
      )
  }

  timeCodeWASResults <- dplyr::bind_rows(timeCovariateCountsBinary, timeCovariateCountsContinuous)

  analysisRef <- covariateCasesControls$analysisRef |> dplyr::collect()

  covariateRef <- covariateCasesControls$covariateRef |> dplyr::collect()

  # get concept_code for covariateRef
  conceptIds <- covariateRef |>
    dplyr::select(conceptId) |>
    dplyr::collect() |>
    dplyr::distinct() |>
    dplyr::rename(concept_id = conceptId)
  conceptIdsTbl <- dplyr::copy_to(connection, conceptIds, overwrite = TRUE, temporary = TRUE)

  conceptIdsAndCodes <- dplyr::tbl(connection, dbplyr::in_schema(vocabularyDatabaseSchema, "concept")) |>
    dplyr::left_join(conceptIdsTbl, by = "concept_id") |>
    dplyr::select(concept_id, concept_code, vocabulary_id, standard_concept) |>
    dplyr::collect() |>
    # rename all to camelCase
    SqlRender::snakeCaseToCamelCaseNames()

  covariateRef <- covariateRef |>
    dplyr::left_join(conceptIdsAndCodes, by = "conceptId") 

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
  databaseInfo <- tibble::tibble(
    databaseId = databaseId,
    databaseName = databaseName,
    databaseDescription = databaseDescription,
    vocabularyVersionCdm = vocabularyVersionCdm,
    vocabularyVersion = vocabularyVersion
  )
  duckdb::dbWriteTable(connection, "databaseInfo", databaseInfo, overwrite = TRUE)

  # Cohort data ------------------------------------------------
  cohortsInfo <- cohortDefinitionSet |>
    dplyr::transmute(
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

  duckdb::dbWriteTable(connection, "cohortsInfo", cohortsInfo, overwrite = TRUE)

  # timeCodeWASCounts ------------------------------------------------
  timeCodeWASResults <- timeCodeWASResults |>
    dplyr::transmute(
      databaseId = as.character({{ databaseId }}),
      covariateId = as.double(covariateId),
      timeId = as.integer(timeId),
      covariateType = as.character(covariateType),
      nCasesYes = as.integer(nCasesYes),
      nCasesInWindow = as.integer(nCasesInWindow),
      meanCases = as.double(meanCases),
      sdCases = as.double(sdCases),
      nControlsYes = as.integer(nControlsYes),
      nControlsInWindow = as.integer(nControlsInWindow),
      meanControls = as.double(meanControls),
      sdControls = as.double(sdControls),
      pValue = as.double(pValue),
      oddsRatio = as.double(oddsRatio),
      modelType = as.character(modelType),
      runNotes = as.character(runNotes)
    )
  duckdb::dbWriteTable(connection, "timeCodeWASResults", timeCodeWASResults, overwrite = TRUE)

  analysisRef <- analysisRef |>
    dplyr::transmute(
      analysisId = as.double(analysisId),
      analysisName = as.character(analysisName),
      domainId = as.character(domainId),
      isBinary = as.character(isBinary),
      missingMeansZero = as.character(missingMeansZero)
    )
  duckdb::dbWriteTable(connection, "analysisRef", analysisRef, overwrite = TRUE)

  covariateRef <- covariateRef |>
    dplyr::transmute(
      covariateId = as.double(covariateId),
      covariateName = as.character(covariateName),
      analysisId = as.double(analysisId),
      conceptId = as.double(conceptId),
      valueAsConceptId = as.double(valueAsConceptId),
      collisions = as.character(collisions),
      conceptCode = as.character(conceptCode),
      vocabularyId = as.character(vocabularyId),
      standardConcept = as.character(standardConcept)
    )
  duckdb::dbWriteTable(connection, "covariateRef", covariateRef, overwrite = TRUE)

  timeRef <- timeWindows |>
    dplyr::transmute(
      timeId = as.double(id_window),
      startDay = as.integer(start_day),
      endDay = as.integer(end_day)
    )
  duckdb::dbWriteTable(connection, "timeRef", timeRef, overwrite = TRUE)

  # analysisInfo ------------------------------------------------
  exportDuration <- Sys.time() - startExportTime

  analysisInfo <- tibble::tibble(
    analysisType = "timeCodeWAS",
    version = "1.1.0",
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
  c("cohortIdCases", "cohortIdControls", "analysisIds", "temporalStartDays", "temporalEndDays") |> checkmate::assertSubset(names(analysisSettings))
  analysisSettings$cohortIdCases |> checkmate::assertNumeric(lower = 1, len = 1)
  analysisSettings$cohortIdControls |> checkmate::assertNumeric(lower = 0, len = 1)
  analysisSettings$analysisIds |> checkmate::assertNumeric()
  analysisSettings$temporalStartDays |> checkmate::assertNumeric()
  analysisSettings$temporalEndDays |> checkmate::assertNumeric()

  if (is.null(analysisSettings$minCellCount)) {
    analysisSettings$minCellCount <- 1
  }
  analysisSettings$minCellCount |> checkmate::assertNumeric()


  return(analysisSettings)
}


#' @title Check TimeCodeWAS Results
#' @description This function checks the integrity and correctness of the exported timeCodeWAS results in a DuckDB database.
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
  expectedSchemas <- list(
    databaseInfo = tibble::tibble(
      name = c("databaseId", "databaseName", "databaseDescription", "vocabularyVersionCdm", "vocabularyVersion"),
      type = c("VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR")
    ),
    cohortsInfo = tibble::tibble(
      name = c("cohortId", "cohortName", "shortName", "sql", "json", "subsetParent", "isSubset", "subsetDefinitionId", "cohortEntries", "cohortSubjects", "use"),
      type = c("INTEGER", "VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR", "INTEGER", "BOOLEAN", "INTEGER", "INTEGER", "INTEGER", "VARCHAR")
    ),
    timeCodeWASResults = tibble::tibble(
      name = c("databaseId", "covariateId", "timeId", "covariateType", "nCasesYes", "nCasesInWindow", "meanCases", "sdCases", "nControlsYes", "nControlsInWindow", "meanControls", "sdControls", "pValue", "oddsRatio", "modelType", "runNotes"),
      type = c("VARCHAR", "DOUBLE", "INTEGER", "VARCHAR", "INTEGER", "INTEGER", "DOUBLE", "DOUBLE", "INTEGER", "INTEGER", "DOUBLE", "DOUBLE", "DOUBLE", "DOUBLE", "VARCHAR", "VARCHAR")
    ),
    analysisRef = tibble::tibble(
      name = c("analysisId", "analysisName", "domainId", "isBinary", "missingMeansZero"),
      type = c("DOUBLE", "VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR")
    ),
    covariateRef = tibble::tibble(
      name = c("covariateId", "covariateName", "analysisId", "conceptId", "valueAsConceptId", "collisions", "conceptCode", "vocabularyId", "standardConcept"),
      type = c("DOUBLE", "VARCHAR", "DOUBLE", "DOUBLE", "DOUBLE", "VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR")
    ),
    analysisInfo = tibble::tibble(
      name = c("analysisType", "version", "analysisSettings", "analysisDuration", "exportDuration"),
      type = c("VARCHAR", "VARCHAR", "VARCHAR", "INTERVAL", "INTERVAL")
    ),
    timeRef = tibble::tibble(
      name = c("timeId", "startDay", "endDay"),
      type = c("DOUBLE", "INTEGER", "INTEGER")
    )
  )

  #
  # Check
  #
  errors <- .checkDatabase(pathToResultsDatabase, expectedSchemas)
  return(errors)
}
