#' @title execute_SubjectsLongitudinalData
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
execute_SubjectsLongitudinalData <- function(
    exportFolder,
    cohortTableHandler,
    analysisSettings) {
  #
  # Check parameters
  #

  exportFolder |> checkmate::assertDirectoryExists()
  cohortTableHandler |> checkmate::assertR6(class = "CohortTableHandler")
  analysisSettings |> assertAnalysisSettings_SubjectsLongitudinalData()


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
  nSubjects <- analysisSettings$nSubjects
  seed <- analysisSettings$seed
  prevalenceTable <- analysisSettings$prevalenceTable

  #
  # function
  #
  ParallelLogger::logInfo("Calculating subjects longitudinal data")
  startAnalysisTime <- Sys.time()

  #
  # Create events table
  #

  # events
  sql <- SqlRender::readSql(system.file("sql/sql_server/CreateEventsTable_SubjectsLongitudinalData.sql", package = "CO2AnalysisModules", mustWork = TRUE))
  sql <- SqlRender::render(
    sql,
    cdm_database_schema = cdmDatabaseSchema,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    cohort_id_cases = cohortIdCases,
    n_subjects = nSubjects,
    seed = seed
  )
  
  if (connection@dbms == "sqlite") {
    sql <- .tmpFixSqliteNoHash(sql)
  }
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  DatabaseConnector::executeSql(connection, sql)

  # measurements
  sql <- SqlRender::readSql(system.file("sql/sql_server/CreateMeasurementsTable_SubjectsLongitudinalData.sql", package = "CO2AnalysisModules", mustWork = TRUE))
  sql <- SqlRender::render(
    sql,
    cdm_database_schema = cdmDatabaseSchema,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    cohort_id_cases = cohortIdCases,
    n_subjects = nSubjects,
    seed = seed
  )
  if (connection@dbms == "sqlite") {
    sql <- .tmpFixSqliteNoHash(sql)
  }
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  DatabaseConnector::executeSql(connection, sql)

  # drugs
  sql <- SqlRender::readSql(system.file("sql/sql_server/CreateDrugsTable_SubjectsLongitudinalData.sql", package = "CO2AnalysisModules", mustWork = TRUE))
  sql <- SqlRender::render(
    sql,
    cdm_database_schema = cdmDatabaseSchema,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    cohort_id_cases = cohortIdCases,
    n_subjects = nSubjects,
    seed = seed
  )
  if (connection@dbms == "sqlite") {
    sql <- .tmpFixSqliteNoHash(sql)
  }
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  DatabaseConnector::executeSql(connection, sql)

  # eras
  sql <- SqlRender::readSql(system.file("sql/sql_server/CreateErasTable_SubjectsLongitudinalData.sql", package = "CO2AnalysisModules", mustWork = TRUE))
  sql <- SqlRender::render(
    sql,
    cdm_database_schema = cdmDatabaseSchema,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    cohort_id_cases = cohortIdCases,
    n_subjects = nSubjects,
    seed = seed
  )
  if (connection@dbms == "sqlite") {
    sql <- .tmpFixSqliteNoHash(sql)
  }
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  DatabaseConnector::executeSql(connection, sql)

  # concept_ancestor
  sql <- SqlRender::readSql(system.file("sql/sql_server/CreateAncestorTable_SubjectsLongitudinalData.sql", package = "CO2AnalysisModules", mustWork = TRUE))
  sql <- SqlRender::render(
    sql,
    cdm_database_schema = cdmDatabaseSchema
  )
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  DatabaseConnector::executeSql(connection, sql)

  # concept
  sql <- SqlRender::readSql(system.file("sql/sql_server/CreateConceptTable_SubjectsLongitudinalData.sql", package = "CO2AnalysisModules", mustWork = TRUE))
  sql <- SqlRender::render(
    sql,
    cdm_database_schema = cdmDatabaseSchema
  )
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  DatabaseConnector::executeSql(connection, sql)

  # prevalence
  if (!is.null(prevalenceTable)) {
    sql <- SqlRender::readSql(system.file("sql/sql_server/CreatePrevalenceTable_SubjectsLongitudinalData.sql", package = "CO2AnalysisModules", mustWork = TRUE))
    sql <- SqlRender::render(
      sql,
      prevalence_table = prevalenceTable
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection, sql)
  }

  analysisDuration <- Sys.time() - startAnalysisTime

  #
  # Export
  #
  ParallelLogger::logInfo("Exporting results")
  startExportTime <- Sys.time()

  pathToResultsDatabase <- file.path(exportFolder, "analysisResults.duckdb")
  duckdbConnection <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  # Database metadata ---------------------------------------------
  databaseInfo <- tibble::tibble(
    databaseId = databaseId,
    databaseName = databaseName,
    databaseDescription = databaseDescription,
    vocabularyVersionCdm = vocabularyVersionCdm,
    vocabularyVersion = vocabularyVersion
  )
  duckdb::dbWriteTable(duckdbConnection, "databaseInfo", databaseInfo, overwrite = TRUE)

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

  duckdb::dbWriteTable(duckdbConnection, "cohortsInfo", cohortsInfo, overwrite = TRUE)

  # events ------------------------------------------------
  events <- DatabaseConnector::dbGetQuery(connection, "SELECT * FROM #events") |> tibble::as_tibble() |> 
    dplyr::mutate(
      person_source_value = as.character(person_source_value),
      event_domain = as.character(event_domain), 
      visit_concept_id = as.double(visit_concept_id),
      visit_source_concept_id = as.double(visit_source_concept_id),
      start_date = as.Date(start_date),
      end_date = as.Date(end_date),
      concept_id = as.double(concept_id),
      source_concept_id = as.double(source_concept_id),
      source_value = as.character(source_value)
    )
  duckdb::dbWriteTable(duckdbConnection, "events", events, overwrite = TRUE)
  rm(events)

  # measurements ------------------------------------------------
  measurements <- DatabaseConnector::dbGetQuery(connection, "SELECT * FROM #measurements") |> tibble::as_tibble() |> 
    dplyr::mutate(
      person_source_value = as.character(person_source_value),
      event_domain = as.character(event_domain), 
      visit_concept_id = as.double(visit_concept_id),
      visit_source_concept_id = as.double(visit_source_concept_id),
      start_date = as.Date(start_date),
      end_date = as.Date(end_date),
      concept_id = as.double(concept_id),
      source_concept_id = as.double(source_concept_id),
      source_value = as.character(source_value),
      value_as_number = as.double(value_as_number),
      value_as_concept_id = as.double(value_as_concept_id),
      unit_concept_id = as.double(unit_concept_id),
      range_low = as.double(range_low),
      range_high = as.double(range_high)
    )
  duckdb::dbWriteTable(duckdbConnection, "measurements", measurements, overwrite = TRUE)
  rm(measurements)

  # drugs ------------------------------------------------
  drugs <- DatabaseConnector::dbGetQuery(connection, "SELECT * FROM #drugs") |> tibble::as_tibble() |> 
    dplyr::mutate(
      person_source_value = as.character(person_source_value),
      event_domain = as.character(event_domain),
      visit_concept_id = as.double(visit_concept_id), 
      visit_source_concept_id = as.double(visit_source_concept_id),
      start_date = as.Date(start_date),
      end_date = as.Date(end_date),
      concept_id = as.double(concept_id),
      source_concept_id = as.double(source_concept_id), 
      source_value = as.character(source_value),
      quantity = as.double(quantity)
    )
  duckdb::dbWriteTable(duckdbConnection, "drugs", drugs, overwrite = TRUE)
  rm(drugs)

  # eras ------------------------------------------------
  eras <- DatabaseConnector::dbGetQuery(connection, "SELECT * FROM #eras") |> tibble::as_tibble() |> 
    dplyr::mutate(
      person_source_value = as.character(person_source_value),
      event_domain = as.character(event_domain),
      start_date = as.Date(start_date),
      end_date = as.Date(end_date),
      concept_id = as.double(concept_id)
    )
  duckdb::dbWriteTable(duckdbConnection, "eras", eras, overwrite = TRUE)
  rm(eras)

  # concept_ancestor ------------------------------------------------
  concept_ancestor <- DatabaseConnector::dbGetQuery(connection, "SELECT * FROM #concept_ancestor") |> tibble::as_tibble() |> 
    dplyr::mutate(
      ancestor_concept_id = as.double(ancestor_concept_id),
      descendant_concept_id = as.double(descendant_concept_id),
      min_levels_of_separation = as.double(min_levels_of_separation),
      max_levels_of_separation = as.double(max_levels_of_separation)
    )
  duckdb::dbWriteTable(duckdbConnection, "concept_ancestor", concept_ancestor, overwrite = TRUE)
  rm(concept_ancestor)

  # concept ------------------------------------------------
  concept <- DatabaseConnector::dbGetQuery(connection, "SELECT * FROM #concept") |> tibble::as_tibble() |> 
    dplyr::mutate(
      concept_id = as.double(concept_id),
      concept_name = as.character(concept_name),
      domain_id = as.character(domain_id),
      vocabulary_id = as.character(vocabulary_id),
      concept_class_id = as.character(concept_class_id),
      standard_concept = as.character(standard_concept),
      concept_code = as.character(concept_code)
    )
  duckdb::dbWriteTable(duckdbConnection, "concept", concept, overwrite = TRUE)
  rm(concept)

  # prevalence ------------------------------------------------
  if (!is.null(prevalenceTable)) {
    prevalence <- DatabaseConnector::dbGetQuery(connection, "SELECT * FROM #prevalence") |> tibble::as_tibble() |> 
      dplyr::mutate(
        source_concept_id = as.double(source_concept_id),
        sex = as.character(sex),
        year_of_birth = as.integer(year_of_birth),
        age_decile = as.integer(age_decile),
        n_persons_with_code = as.integer(n_persons_with_code),
        n_persons_with_observation = as.integer(n_persons_with_observation)
      )
  }else {
    prevalence <- tibble::tibble(
      source_concept_id = as.double(NA),
      sex = as.character(NA),
      year_of_birth = as.integer(NA),
      age_decile = as.integer(NA),
      n_persons_with_code = as.integer(NA),
      n_persons_with_observation = as.integer(NA), 
      .rows = 0
    )
  }
  duckdb::dbWriteTable(duckdbConnection, "prevalence", prevalence, overwrite = TRUE)
  rm(prevalence)


  # analysisInfo ------------------------------------------------
  exportDuration <- Sys.time() - startExportTime

  analysisInfo <- tibble::tibble(
    analysisType = "SubjectsLongitudinalData",
    version = "1.0.0",
    analysisSettings = yaml::as.yaml(analysisSettings),
    analysisDuration = analysisDuration,
    exportDuration = exportDuration
  )
  duckdb::dbWriteTable(duckdbConnection, "analysisInfo", analysisInfo, overwrite = TRUE)

  # close connection
  DatabaseConnector::disconnect(connection)
  duckdb::dbDisconnect(duckdbConnection)

  ParallelLogger::logInfo("Results exported")

  return(pathToResultsDatabase)
}

#' @title Assert Analysis Settings for Cohort Demographics Overlaps
#' @description Validates the `analysisSettings` list to ensure it contains the required elements (`cohortIds`, `referenceYears`, `groupBy`, `minCellCount`) with correct types and values. This function is specifically designed for checking settings related to cohort demographics analysis.
#'
#' @param analysisSettings A list containing analysis settings. It must include the following elements:
#' \describe{
#'   \item{cohortIdCases}{A numeric value representing the cohort ID for cases.}
#'   \item{cohortIdControls}{A numeric value representing the cohort ID for controls.}
#'   \item{nSubjects}{A numeric value representing the number of subjects to sample.}
#'   \item{seed}{A numeric value representing the seed for the random number generator.}
#' }
#'
#' @return Returns `TRUE` if all settings are valid; otherwise, throws an error.
#'
#' @importFrom checkmate assertList assertSubset assertNumeric assertCharacter
#'
#' @export
assertAnalysisSettings_SubjectsLongitudinalData <- function(analysisSettings) {
  analysisSettings |> checkmate::assertList()
  analysisSettings |>
    names() |>
    checkmate::assertSubset(c("cohortIdCases", "cohortIdControls", "nSubjects", "seed", "prevalenceTable"))
  analysisSettings$cohortIdCases |> checkmate::assertNumber(lower = 1)
  analysisSettings$cohortIdControls |> checkmate::assertNumber(lower = 1, null.ok = TRUE)
  if (is.null(analysisSettings$cohortIdControls)) {
    analysisSettings$cohortIdControls <- 100
  }
  analysisSettings$nSubjects |> checkmate::assertNumber(lower = 1)
  if (is.null(analysisSettings$seed)) {
    analysisSettings$seed <- 123
  }
  analysisSettings$seed |> checkmate::assertNumber(lower = 1)
  analysisSettings$prevalenceTable |> checkmate::assertCharacter(null.ok = TRUE)
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
checkResults_SubjectsLongitudinalData <- function(pathToResultsDatabase) {
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
    events = tibble::tibble(
      name = c("person_source_value", "event_domain", "visit_concept_id", "visit_source_concept_id", "start_date", "end_date", "concept_id", "source_concept_id", "source_value"),
      type = c("VARCHAR", "VARCHAR", "DOUBLE", "DOUBLE", "DATE", "DATE", "DOUBLE", "DOUBLE", "VARCHAR")
    ),
    measurements = tibble::tibble(
      name = c("person_source_value", "event_domain", "visit_concept_id", "visit_source_concept_id", "start_date", "end_date", "concept_id", "source_concept_id", "source_value", "value_as_number", "value_as_concept_id", "unit_concept_id", "range_low", "range_high"),
      type = c("VARCHAR", "VARCHAR", "DOUBLE", "DOUBLE", "DATE", "DATE", "DOUBLE", "DOUBLE", "VARCHAR", "DOUBLE", "DOUBLE", "DOUBLE", "DOUBLE", "DOUBLE")
    ),
    drugs = tibble::tibble(
      name = c("person_source_value", "event_domain", "visit_concept_id", "visit_source_concept_id", "start_date", "end_date", "concept_id", "source_concept_id", "source_value", "quantity"),
      type = c("VARCHAR", "VARCHAR", "DOUBLE", "DOUBLE", "DATE", "DATE", "DOUBLE", "DOUBLE", "VARCHAR", "DOUBLE")
    ),
    eras = tibble::tibble(
      name = c("person_source_value", "event_domain", "start_date", "end_date", "concept_id"),
      type = c("VARCHAR", "VARCHAR", "DATE", "DATE", "DOUBLE")
    ),
    concept_ancestor = tibble::tibble(
      name = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
      type = c("DOUBLE", "DOUBLE", "DOUBLE", "DOUBLE")
    ),
    concept = tibble::tibble(
      name = c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code"),
      type = c("DOUBLE", "VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR", "VARCHAR")
    ),
    prevalence = tibble::tibble(
      name = c("source_concept_id", "sex", "year_of_birth", "age_decile", "n_persons_with_code", "n_persons_with_observation"),
      type = c("DOUBLE", "VARCHAR", "INTEGER", "INTEGER", "INTEGER", "INTEGER")
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


.tmpFixSqliteNoHash <- function(sql) {
  sql  <- stringr::str_replace(sql, "HASHBYTES\\('MD5'.*\\)", "person_source_value")
  return(sql)
}
