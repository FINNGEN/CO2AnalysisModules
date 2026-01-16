#' @title execute_PhenotypeScoring
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
#' @importFrom HadesExtras getCohortNamesFromCohortDefinitionTable
#' @importFrom FeatureExtraction createCohortBasedTemporalCovariateSettings getDbCovariateData
#' @importFrom HadesExtras FeatureExtraction_createTemporalCovariateSettingsFromList
#' @importFrom SqlRender render translate
#' @importFrom DatabaseConnector dbExecute
#' @importFrom tidyr spread
#' @importFrom purrr map
#' @importFrom utils capture.output
#' @importFrom speedglm speedglm
#' @importFrom stringr str_detect
#' @importFrom HadesExtras getCohortNamesFromCohortDefinitionTable
#' @importFrom stats binomial gaussian sd as.formula na.omit pt setNames t.test chisq.test
#'
#' @export
#'
execute_PhenotypeScoring <- function(
    exportFolder,
    cohortTableHandler,
    analysisSettings) {
  #
  # Check parameters
  #
  exportFolder |> checkmate::assertDirectoryExists()
  cohortTableHandler |> checkmate::assertR6(class = "CohortTableHandler")
  analysisSettings <- analysisSettings |> assertAnalysisSettings_PhenotypeScoring()


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
  autoMatchRatio <- analysisSettings$autoMatchRatio
  analysisIds <- analysisSettings$analysisIds

  #
  # Calculate a codeWas
  #
  startAnalysisTime <- Sys.time()
  ParallelLogger::logInfo("Calculating codeWas")

  analysisSettings <- list(
    cohortIdCases = cohortIdCases,
    cohortIdControls = cohortIdControls,
    autoMatchRatio=autoMatchRatio,
    analysisIds = analysisIds,
    covariatesIds = NULL,
    minCellCount = 1
  )

  pathToResultsDatabase <- execute_CodeWAS(
    exportFolder = exportFolder,
    cohortTableHandler = cohortTableHandler,
    analysisSettings = analysisSettings
  )

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  codewasResultsTbl <-
    analysisResults |>
    dplyr::tbl("codewasResults")

  covariateRefTbl <-
    analysisResults |>
    dplyr::tbl("covariateRef")

  analysisRefTbl <-
    analysisResults |>
    dplyr::tbl("analysisRef")

  #
  # Get counts
  #

  ParallelLogger::logInfo("Getting covariate ids")

  pValueThreshold <- 0.05
  oddRatioThreshold <- 0.1
  nCasesThreshold <- 10

  ATCLevelThreshold <- 7

  # Apply filter if not demographics domain
  covariates <- codewasResultsTbl |>
    dplyr::select(covariateId, covariateType, nCasesYes, pValue, oddsRatio) |>
    dplyr::left_join(
      covariateRefTbl |>
        dplyr::select(analysisId, covariateId, conceptId, conceptCode, vocabularyId),
      by = c("covariateId" = "covariateId")
    ) |>
    dplyr::left_join(
      analysisRefTbl |>
        dplyr::select(analysisId, domainId),
      by = c("analysisId" = "analysisId")
    )

  covariatesNoDemographics <- covariates |>
    dplyr::filter(domainId != "Demographics") |>
    dplyr::filter(pValue < pValueThreshold) |>
    dplyr::filter(oddsRatio > oddRatioThreshold) |>
    dplyr::filter(nCasesYes > nCasesThreshold) |>
    dplyr::filter(!(vocabularyId == "ATC" & nchar(conceptCode) < ATCLevelThreshold)) |>
    dplyr::distinct(analysisId, covariateId, conceptId) |>
    dplyr::collect() |>
    dplyr::left_join(
      HadesExtras::getListOfAnalysis(),
      by = c("analysisId" = "analysisId")
    ) |>
    dplyr::select(analysisId, domainId, covariateId, conceptId, isBinary, isSourceConcept)

  covariatesDemographics <- covariates |>
    dplyr::filter(domainId == "Demographics") |>
    dplyr::collect()

  duckdb::dbDisconnect(analysisResults)

  #
  # Non demographics analysis
  #
  ParallelLogger::logInfo("Extracting covariates per person")
  covariatesPerPerson <- .extractCovariatesPerPerson(
    cohortTableHandler = cohortTableHandler,
    cohortId = cohortIdCases,
    covariatesTable = covariatesNoDemographics
  )

  #
  # Demographics
  #
  ParallelLogger::logInfo("Getting FeatureExtraction for cases and controls")

  covariateSettings <- HadesExtras::FeatureExtraction_createTemporalCovariateSettingsFromList(
    analysisIds = unique(covariatesDemographics$analysisId),
    temporalStartDays = -99999,
    temporalEndDays = 99999
  )

  demographicsResults <- FeatureExtraction::getDbCovariateData(
      connection = connection,
      cohortTable = cohortTable,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cdmDatabaseSchema = cdmDatabaseSchema,
      covariateSettings = covariateSettings,
      cohortIds = c(cohortIdCases),
      aggregated = FALSE,
      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
    )

  # get the pesont_id to source_person_id for the cohortIdCases
  sql <- "
  SELECT DISTINCT
    person.person_id,
    person.person_source_value AS source_person_id
  FROM @cohort_database_schema.@cohort_table cohort
  INNER JOIN @cdm_database_schema.person person
    ON cohort.subject_id = person.person_id
  WHERE cohort.cohort_definition_id = @cohort_id"

  sql <- SqlRender::render(sql,
    cdm_database_schema = cdmDatabaseSchema,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    cohort_id = cohortIdCases,
    warnOnMissingParameters = TRUE
  )

  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)

  personIdToSourcePersonId <- DatabaseConnector::querySql(connection, sql) |>
    dplyr::as_tibble() |>
    SqlRender::snakeCaseToCamelCaseNames()

  covariatesPerPersonDemographics <- demographicsResults$covariates |>
    dplyr::collect() |>
    dplyr::left_join(personIdToSourcePersonId, by = c("rowId" = "personId"))  |>
    dplyr::transmute(
      personSourceValue = as.character(sourcePersonId),
      covariateId = as.double(covariateId),
      value = as.double(covariateValue),
      unit = dplyr::case_when(
      covariateId == 8507001 ~ "yes/no",
      covariateId == 8532001 ~ "yes/no",
      covariateId == 1002 ~ "years",
      covariateId == 1041 ~ "year",
      covariateId == 1010 ~ "days",
      TRUE ~ ""
    )
  )

  covariatesPerPerson <- dplyr::bind_rows(covariatesPerPerson, covariatesPerPersonDemographics)


  ParallelLogger::logInfo("PhenotypeScoring completed")
  analysisDuration <- Sys.time() - startAnalysisTime


  #
  # Export
  #
  ParallelLogger::logInfo("Exporting results")
  startExportTime <- Sys.time()

  connection <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  # covariatesPerPerson ------------------------------------------------
  covariatesPerPerson <- covariatesPerPerson |>
    dplyr::mutate(
      personSourceValue = as.character(personSourceValue),
      covariateId = as.double(covariateId),
      value = as.double(value),
      unit = as.character(unit)
    )

  duckdb::dbWriteTable(connection, "covariatesPerPerson", covariatesPerPerson, overwrite = TRUE)

  # analysisInfo ------------------------------------------------
  exportDuration <- Sys.time() - startExportTime

  analysisInfo <- tibble::tibble(
    analysisType = "phenotypeScoring",
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

#' @title Assert Analysis Settings for PhenotypeScoring
#' @description Validates the `analysisSettings` list to ensure it contains the required elements (`cohortIdCases`, `cohortIdControls`, `analysisIds`, `covariatesIds`, `minCellCount`, `chunksSizeNOutcomes`, `cores`) with correct types and values. This function is specifically designed for checking settings related to PhenotypeScoring analysis.
#'
#' @param analysisSettings A list containing analysis settings. It must include the following elements:
#' \describe{
#'   \item{cohortIdCases}{A numeric value representing the cohort ID for cases.}
#'   \item{cohortIdControls}{A numeric value representing the cohort ID for controls.}
#'   \item{analysisIds}{A character vector of analysis IDs.}
#'   \item{covariatesIds}{A numeric vector of covariate IDs (optional).}
#'   \item{minCellCount}{A numeric value representing the minimum cell count, must be 0 or higher.}
#'   \item{chunksSizeNOutcomes}{A numeric value representing the chunk size for outcomes (optional).}
#'   \item{cores}{A numeric value representing the number of cores to use for parallel processing.}
#' }
#'
#' @return Returns the validated `analysisSettings` list.
#'
#' @importFrom checkmate assertList assertSubset assertNumeric
#'
#' @export
assertAnalysisSettings_PhenotypeScoring <- function(analysisSettings) {
  analysisSettings |> checkmate::assertList()
  c("cohortIdCases", "cohortIdControls", "analysisIds") |> checkmate::assertSubset(names(analysisSettings))
  analysisSettings$cohortIdCases |> checkmate::assertNumeric(lower = 1, len = 1)
  analysisSettings$cohortIdControls |> checkmate::assertNumeric(lower = 0, len = 1)
  analysisSettings$analysisIds |> checkmate::assertNumeric()

  analysisSettings$analysisId |> checkmate::assertSubset(
    choices = c(
      141, # source condition counts
      342, # ATC group counts
      1, # DemographicsGender
      2, # DemographicsAge
      10, # DemographicsTimeInCohort
      41 # year of birth
    )
  )

  return(analysisSettings)
}


#' @title Check PhenotypeScoring Results
#' @description This function checks the integrity and correctness of the exported PhenotypeScoring results in a DuckDB database.
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
checkResults_PhenotypeScoring <- function(pathToResultsDatabase) {
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
    codewasResults = tibble::tibble(
      name = c("databaseId", "covariateId", "covariateType", "nCasesYes", "meanCases", "sdCases", "nControlsYes", "meanControls", "sdControls", "pValue", "oddsRatio", "modelType", "runNotes"),
      type = c("VARCHAR", "DOUBLE", "VARCHAR", "INTEGER", "DOUBLE", "DOUBLE", "INTEGER", "DOUBLE", "DOUBLE", "DOUBLE", "DOUBLE", "VARCHAR", "VARCHAR")
    ),
    covariatesPerPerson = tibble::tibble(
      name = c("personSourceValue", "covariateId", "value", "unit"),
      type = c("VARCHAR", "DOUBLE", "DOUBLE", "VARCHAR")
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
    )
  )

  #
  # Check
  #
  errors <- .checkDatabase(pathToResultsDatabase, expectedSchemas)
  return(errors)
}


.extractCovariatesPerPerson <- function(
    cohortTableHandler,
    cohortId,
    covariatesTable) {
  connection <- cohortTableHandler$connectionHandler$getConnection()
  cdmDatabaseSchema <- cohortTableHandler$cdmDatabaseSchema
  cohortDatabaseSchema <- cohortTableHandler$cohortDatabaseSchema
  cohortTable <- cohortTableHandler$cohortTableNames$cohortTable

  domainTablesInfo <- tibble::tibble(
    domainId = c("Condition", "Drug", "Procedure", "Measurement"),
    domainTable = c("condition_occurrence", "drug_exposure", "procedure_occurrence", "measurement"),
    domainConceptId = c("condition_concept_id", "drug_concept_id", "procedure_concept_id", "measurement_concept_id"),
    domainSourceConceptId = c("condition_source_concept_id", "drug_source_concept_id", "procedure_source_concept_id", "measurement_source_concept_id"),
    domainEndDate = c("condition_end_date", "drug_exposure_end_date", "procedure_end_date", "measurement_date")
  )

  covariatesTableTemp <- covariatesTable |>
    dplyr::select(analysisId, covariateId, conceptId) |>
    SqlRender::camelCaseToSnakeCaseNames()

  # Connect to tables and copy cohortData to database
  DatabaseConnector::insertTable(
    connection = connection,
    tableName = "covariates_table",
    data = covariatesTableTemp,
    tempTable = TRUE,
    dropTableIfExists = TRUE
  )

  covariatesPerPerson <- tibble::tibble()

  for (analysisId in unique(covariatesTable$analysisId)) {
    analysisInfo <- covariatesTable |>
      dplyr::filter(analysisId == {{ analysisId }})

    domainId <- analysisInfo$domainId[1]
    isBinary <- analysisInfo$isBinary[1]
    isSourceConcept <- analysisInfo$isSourceConcept[1]

    ParallelLogger::logInfo(paste0("Extracting covariates for analysisId: ", analysisId))
    ParallelLogger::logInfo(paste0("Domain: ", domainId, " Isbinary: ", isBinary, " IsSourceConcept: ", isSourceConcept, " Number of covariates: ", nrow(analysisInfo)))


    #
    # Binary covariates no drugs
    #
    if (isBinary && domainId != "Drug") {

      ParallelLogger::logInfo(paste0("Binary covariates no drugs"))

      tableInfo <- domainTablesInfo |>
        dplyr::filter(domainId == {{ domainId }})

      domainTable <- tableInfo$domainTable
      domainEndDate <- tableInfo$domainEndDate
      domainConceptId <- ifelse(isSourceConcept, tableInfo$domainSourceConceptId, tableInfo$domainConceptId)

      sql <- "
      SELECT DISTINCT
          person.person_source_value as person_source_value,
          cov.covariate_id as covariate_id,
          COUNT(DISTINCT events.visit_occurrence_id) as value,
          'count' as unit
      FROM @cohort_database_schema.@cohort_table cohort
      INNER JOIN @cdm_database_schema.person person
          ON cohort.subject_id = person.person_id
      INNER JOIN @cdm_database_schema.@domain_table events
          ON person.person_id = events.person_id
      INNER JOIN #covariates_table cov
          ON events.@domain_concept_id = cov.concept_id
      WHERE cohort.cohort_definition_id = @cohort_id AND
          cov.analysis_id = @analysis_id AND
          events.@domain_end_date IS NOT NULL -- Feature extraction does not include events with null end date
      GROUP BY
          person.person_source_value,
          cov.covariate_id
  "

      sql <- SqlRender::render(sql,
        cdm_database_schema = cdmDatabaseSchema,
        cohort_database_schema = cohortDatabaseSchema,
        cohort_table = cohortTable,
        cohort_id = cohortId,
        analysis_id = analysisId,
        domain_table = domainTable,
        domain_concept_id = domainConceptId,
        domain_end_date = domainEndDate,
        warnOnMissingParameters = TRUE
      )

      sql <- SqlRender::translate(sql, targetDialect = connection@dbms)

      covariates <- DatabaseConnector::querySql(connection, sql) |>
        dplyr::as_tibble() |>
        SqlRender::snakeCaseToCamelCaseNames()

      covariatesPerPerson <- dplyr::bind_rows(covariatesPerPerson, covariates)
    }

    #
    # Binary covariates with drugs
    #
    if (isBinary && domainId == "Drug") {
      ParallelLogger::logInfo(paste0("Binary covariates with drugs"))

      tableInfo <- domainTablesInfo |>
        dplyr::filter(domainId == {{ domainId }})

      domainTable <- tableInfo$domainTable
      domainConceptId <- tableInfo$domainConceptId
      domainEndDate <- tableInfo$domainEndDate
      sql <- "
      SELECT DISTINCT
          person.person_source_value as person_source_value,
          cov.covariate_id as covariate_id,
          COUNT(DISTINCT events.visit_occurrence_id) as value,
          'count' as unit
      FROM @cohort_database_schema.@cohort_table cohort
      INNER JOIN @cdm_database_schema.person person
          ON cohort.subject_id = person.person_id
      INNER JOIN @cdm_database_schema.@domain_table events
          ON person.person_id = events.person_id
      INNER JOIN @cdm_database_schema.concept_ancestor ca
        ON events.@domain_concept_id = ca.descendant_concept_id
      INNER JOIN #covariates_table cov
          ON ca.ancestor_concept_id = cov.concept_id
      WHERE cohort.cohort_definition_id = @cohort_id AND
          cov.analysis_id = @analysis_id AND
          events.@domain_end_date IS NOT NULL -- Feature extraction does not include events with null end date
      GROUP BY
          person.person_source_value,
          cov.covariate_id
  "

      sql <- SqlRender::render(sql,
        cdm_database_schema = cdmDatabaseSchema,
        cohort_database_schema = cohortDatabaseSchema,
        cohort_table = cohortTable,
        cohort_id = cohortId,
        analysis_id = analysisId,
        domain_table = domainTable,
        domain_concept_id = domainConceptId,
        domain_end_date = domainEndDate,
        warnOnMissingParameters = TRUE
      )

      sql <- SqlRender::translate(sql, targetDialect = connection@dbms)

      covariates <- DatabaseConnector::querySql(connection, sql) |>
        dplyr::as_tibble() |>
        SqlRender::snakeCaseToCamelCaseNames()

      covariatesPerPerson <- dplyr::bind_rows(covariatesPerPerson, covariates)
    }

  }

  # TEMP: visit with null values result in 0 counts, make them at least 1
  covariatesPerPerson <- covariatesPerPerson |>
    dplyr::mutate(value = dplyr::if_else(value == 0, 1, value))


  return(covariatesPerPerson)
}
