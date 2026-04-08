#' @title execute_CodeWAS
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
execute_CodeWAS <- function(
    exportFolder,
    cohortTableHandler,
    analysisSettings) {
  #
  # Check parameters
  #
  exportFolder |> checkmate::assertDirectoryExists()
  cohortTableHandler |> checkmate::assertR6(class = "CohortTableHandler")
  analysisSettings <- analysisSettings |> assertAnalysisSettings_CodeWAS()


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
  covariatesIds <- analysisSettings$covariatesIds
  minCellCount <- analysisSettings$minCellCount
  chunksSizeNOutcomes <- analysisSettings$chunksSizeNOutcomes
  cores <- analysisSettings$cores
  analysisRegexTibble <- analysisSettings$analysisRegexTibble

  covariatesNoAnalysis <- setdiff(covariatesIds %% 1000, analysisIds)
  if (length(covariatesNoAnalysis) > 0) {
    stop("The following covariates do not have an associated analysis: ", covariatesNoAnalysis)
  }

  #
  # if cohortIdControls is 0 create the control cohort first
  #
  cohortsToDelete <- c()
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

    # Match to sex and bday, with default match ratio 10 or user selected matched ratio
    subsetDef <- CohortGenerator::createCohortSubsetDefinition(
      name = "match",
      definitionId = cohortIdCases,
      subsetOperators = list(
        HadesExtras::createMatchingSubset(
          matchToCohortId = cohortIdCases,
          #matchRatio = 10,
          matchRatio = autoMatchRatio,
          matchSex = TRUE,
          matchBirthYear = TRUE,
          matchCohortStartDateWithInDuration = FALSE,
          newCohortStartDate = "keep",
          newCohortEndDate = "keep"
        )
      )
    )

    cohortDefinitionSet <- cohortDefinitionSet |>
      CohortGenerator::addCohortSubsetDefinition(subsetDef, targetCohortIds = newCohortId)

    cohortDefinitionSet <- cohortDefinitionSet |>
      dplyr::mutate(shortName = dplyr::if_else(cohortId == newCohortId*1000 + cohortIdCases, paste0("Mx", newCohortShortName), shortName))

    cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)

    cohortIdControls <- newCohortId*1000 + cohortIdCases
    cohortDefinitionSet  <- cohortTableHandler$cohortDefinitionSet
    cohortsToDelete  <- c(newCohortId, cohortIdControls)
  }

  #
  # function
  #
  ParallelLogger::logInfo("Calculating CodeWAS")
  startAnalysisTime <- Sys.time()

  noCovariatesMode <- is.null(covariatesIds) || length(covariatesIds) == 0

  allRegexAnalysisIds <- {
    if (!is.null(analysisRegexTibble)) analysisRegexTibble |> dplyr::pull(analysisId) else c()
  }

  # separate analysisIds into default and regex, default are the ones that are not in the regex
  defaultAnalysisIds <- analysisIds |> setdiff(allRegexAnalysisIds)
  regexAnalysisIds <- analysisIds |> intersect(allRegexAnalysisIds)

  # numeric analysis settings
  covariateSettings <- HadesExtras::FeatureExtraction_createTemporalCovariateSettingsFromList(
    analysisIds = defaultAnalysisIds,
    temporalStartDays = -99999,
    temporalEndDays = 99999
  )

  # regex analysis setting
  if (length(regexAnalysisIds) > 0) {
    cohortDefinitionTable <- HadesExtras::getCohortNamesFromCohortDefinitionTable(
      connection = connection,
      cohortDatabaseSchema = cdmDatabaseSchema
    )

    selectedanalysisRegexTibble <- analysisRegexTibble |>
      dplyr::filter(analysisId %in% regexAnalysisIds)

    for (i in 1:nrow(selectedanalysisRegexTibble)) {
      analysisId <- selectedanalysisRegexTibble$analysisId[i]
      analysisRegex <- selectedanalysisRegexTibble$analysisRegex[i]

      covariateCohorts <- cohortDefinitionTable |>
        dplyr::filter(grepl(analysisRegex, cohort_definition_name, perl = TRUE)) |>
        dplyr::transmute(
          cohortId = cohort_definition_id,
          cohortName = cohort_definition_description
        )

      if (nrow(covariateCohorts) == 0) {
        ParallelLogger::logInfo("No cohorts found for analysisId ", analysisId, " with regex ", analysisRegex)
        next
      }

      covariateSettingsCohortBased <- FeatureExtraction::createCohortBasedTemporalCovariateSettings(
        analysisId = analysisId,
        covariateCohortDatabaseSchema = cdmDatabaseSchema,
        covariateCohortTable = "cohort",
        covariateCohorts = covariateCohorts,
        valueType = "binary",
        temporalStartDays = -99999,
        temporalEndDays = 99999
      )

      covariateSettings[[length(covariateSettings) + 1]] <- covariateSettingsCohortBased
    }
  }


  cohortCounts <- cohortTableHandler$getCohortCounts()
  nCasesTotal <- cohortCounts$cohortSubjects[cohortCounts$cohortId == cohortIdCases]
  nControlsTotal <- cohortCounts$cohortSubjects[cohortCounts$cohortId == cohortIdControls]
  nCasesEntries <- cohortCounts$cohortEntries[cohortCounts$cohortId == cohortIdCases]
  nControlsEntries <- cohortCounts$cohortEntries[cohortCounts$cohortId == cohortIdControls]

  # if the number of entries is different that the number of subjects, cohort needs to be distinct
  if (nCasesEntries != nCasesTotal | nControlsEntries != nControlsTotal) {
    ParallelLogger::logInfo("Number of entries is different than number of subjects, cohort needs to be distinct in a temp table")

    newCohortTable <- paste0(cohortTable, "_distinct")

    sql <- "
    DROP TABLE IF EXISTS @cohort_database_schema.@newCohortTable;
    CREATE TABLE @cohort_database_schema.@newCohortTable AS
    SELECT *
    FROM (
        SELECT *,
               ROW_NUMBER() OVER (PARTITION BY cohort_definition_id, subject_id ORDER BY cohort_start_date) AS rn
        FROM @cohort_database_schema.@cohortTable
    ) sub
    WHERE rn = 1;
    "

    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      cohort_database_schema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      newCohortTable = newCohortTable,
      warnOnMissingParameters = TRUE
    )

    cohortTable <- newCohortTable
  }

  #
  # noCovariatesMode
  #
  if (noCovariatesMode) {
    ParallelLogger::logInfo("Running regresions without covariates")

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
    covariateCountsBinary <- tibble::tibble()
    if (!is.null(covariateCasesControls$covariates)) {
      ParallelLogger::logInfo("Running statistical test for binary covariates")

      covariateCountsBinary <- dplyr::full_join(
        covariateCasesControls$covariates |>
          dplyr::filter(cohortDefinitionId == {{ cohortIdCases }}) |>
          dplyr::select(covariateId, nCasesYes = sumValue),
        covariateCasesControls$covariates |>
          dplyr::filter(cohortDefinitionId == {{ cohortIdControls }}) |>
          dplyr::select(covariateId, nControlsYes = sumValue),
        by = "covariateId"
      ) |>
        dplyr::mutate(
          nCasesYes = ifelse(is.na(nCasesYes), 0, nCasesYes),
          nControlsYes = ifelse(is.na(nControlsYes), 0, nControlsYes),
          nCasesTotal = {{ nCasesTotal }},
          nControlsTotal = {{ nControlsTotal }},
          nCasesNo = nCasesTotal - nCasesYes,
          nControlsNo = nControlsTotal - nControlsYes
        ) |>
        dplyr::collect()

      covariateCountsBinary <- .addTestToCodeCounts(covariateCountsBinary) |>
        dplyr::transmute(
          covariateId = covariateId,
          covariateType = "binary",
          nCasesYes = nCasesYes,
          meanCases = nCasesYes / nCasesTotal,
          sdCases = sqrt(meanCases * (1 - meanCases)),
          nControlsYes = nControlsYes,
          meanControls = nControlsYes / nControlsTotal,
          sdControls = sqrt(meanControls * (1 - meanControls)),
          pValue = countsPValue,
          oddsRatio = dplyr::if_else(is.infinite(countsOddsRatio), .Machine$double.xmax, countsOddsRatio),
          modelType = countsTest,
          runNotes = ""
        )
    }

    # continuous
    covariateCountsContinuous <- tibble::tibble()
    if (!is.null(covariateCasesControls$covariatesContinuous) && covariateCasesControls$covariatesContinuous |> dplyr::count() |> dplyr::pull(n) != 0) {
      ParallelLogger::logInfo("Running statistical test for continuous covariates")

      covariateCountsContinuous <- dplyr::full_join(
        covariateCasesControls$covariatesContinuous |>
          dplyr::filter(cohortDefinitionId == {{ cohortIdCases }}) |>
          dplyr::select(covariateId, nCasesYesWithValue = countValue, meanValueCases = averageValue, sdValueCases = standardDeviation),
        covariateCasesControls$covariatesContinuous |>
          dplyr::filter(cohortDefinitionId == {{ cohortIdControls }}) |>
          dplyr::select(covariateId, nControlsYesWithValue = countValue, meanValueControls = averageValue, sdValueControls = standardDeviation),
        by = "covariateId"
      ) |>
        dplyr::mutate(
          nCasesYesWithValue = ifelse(is.na(nCasesYesWithValue), 0, nCasesYesWithValue),
          nControlsYesWithValue = ifelse(is.na(nControlsYesWithValue), 0, nControlsYesWithValue),
          meanValueCases = ifelse(is.na(meanValueCases), 0, meanValueCases),
        ) |>
        dplyr::collect()

      covariateCountsContinuous <- .addTestTotibbleWithValueSummary(covariateCountsContinuous) |>
        dplyr::transmute(
          covariateId = covariateId,
          covariateType = "continuous",
          nCasesYes = nCasesYesWithValue,
          meanCases = meanValueCases,
          sdCases = sdValueCases,
          nControlsYes = nControlsYesWithValue,
          meanControls = meanValueControls,
          sdControls = sdValueControls,
          pValue = continuousPValue,
          oddsRatio = dplyr::if_else(is.infinite(continuousOddsRatio), .Machine$double.xmax, continuousOddsRatio),
          modelType = continuousTest,
          runNotes = ""
        )
    }

    codewasResults <- dplyr::bind_rows(covariateCountsBinary, covariateCountsContinuous)

    analysisRef <- covariateCasesControls$analysisRef |> dplyr::collect()

    covariateRef <- covariateCasesControls$covariateRef |> dplyr::collect()
  } else {
    #
    # CovariatesMode
    #
    ParallelLogger::logInfo("Running regresions with covariates")

    ParallelLogger::logInfo("Getting FeatureExtraction for cases")
    covariate_case <- FeatureExtraction::getDbCovariateData(
      connection = connection,
      cohortTable = cohortTable,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cdmDatabaseSchema = cdmDatabaseSchema,
      covariateSettings = covariateSettings,
      cohortIds = cohortIdCases,
      aggregated = F,
      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
    )
    ParallelLogger::logInfo("Getting FeatureExtraction for controls")
    covariate_control <- FeatureExtraction::getDbCovariateData(
      connection = connection,
      cohortTable = cohortTable,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cdmDatabaseSchema = cdmDatabaseSchema,
      covariateSettings = covariateSettings,
      cohortIds = cohortIdControls,
      aggregated = F,
      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
    )

    # case control covariates
    covarCase <- covariate_case$covariates |>
      dplyr::distinct(rowId) |>
      dplyr::collect() |>
      dplyr::transmute(personId = rowId, covariateId = "caseControl", covariateValue = TRUE)

    covarControl <- covariate_control$covariates |>
      dplyr::distinct(rowId) |>
      dplyr::collect() |>
      dplyr::transmute(personId = rowId, covariateId = "caseControl", covariateValue = FALSE)

    # error if no patients in cases
    if (nrow(covarCase) == 0) {
      stop("Cases dont have any covariates to compare")
    }
    # error if no patients in controls
    if (nrow(covarControl) == 0) {
      stop("Controls dont have any covariates to compare")
    }

    # warning if patient ids overlap
    nOverlap <- length(intersect(covarCase$personId, covarControl$personId))
    if (nOverlap > 0) {
      warning("There are ", nOverlap, " patients that are in both cases and controls, cases were removed from controls.")
      covarControl <- covarControl |> dplyr::filter(!personId %in% covarCase$personId)
    }

    caseControlCovariateWide <- dplyr::bind_rows(covarCase, covarControl) |>
      tidyr::spread(covariateId, covariateValue)


    #
    # break analysis into chunks of covariates
    #
    allCovariates <- dplyr::bind_rows(
      covariate_case$covariates |>
        dplyr::left_join(covariate_case$covariateRef, by = "covariateId") |>
        dplyr::left_join(covariate_case$analysisRef, by = "analysisId") |>
        dplyr::distinct(covariateId, isBinary) |>
        dplyr::collect(),
      covariate_control$covariates |>
        dplyr::left_join(covariate_control$covariateRef, by = "covariateId") |>
        dplyr::left_join(covariate_control$analysisRef, by = "analysisId") |>
        dplyr::distinct(covariateId, isBinary) |>
        dplyr::collect()
    ) |> dplyr::distinct()

    codewasResults <- tibble::tibble()

    ParallelLogger::logInfo("Running ", nrow(allCovariates), " covariates in ", length(seq(1, nrow(allCovariates), chunksSizeNOutcomes)), " chunks of ", chunksSizeNOutcomes, " covariates")
    for (i in seq(1, nrow(allCovariates), chunksSizeNOutcomes)) {
      ParallelLogger::logInfo("Running chunk ", i, " to ", min(i + chunksSizeNOutcomes - 1, nrow(allCovariates)))
      covariateIdsChunk <- allCovariates |>
        dplyr::slice(i:min(i + chunksSizeNOutcomes - 1, nrow(allCovariates))) |>
        dplyr::pull(covariateId)
      # add controling covariates
      covariateIdsChunk <- union(covariateIdsChunk, covariatesIds)

      BinaryCovariateNames <- covariateIdsChunk |>
        intersect(allCovariates$covariateId[allCovariates$isBinary == "Y"]) |>
        as.character()

      ParallelLogger::logInfo("Prepare data for ", length(covariateIdsChunk), " covariates")

      # covariates
      covariatesWideTable <- dplyr::bind_rows(
        covariate_case$covariates |>
          dplyr::filter(covariateId %in% covariateIdsChunk) |>
          dplyr::distinct(rowId, covariateId, covariateValue) |>
          dplyr::collect(),
        covariate_control$covariates |>
          dplyr::filter(covariateId %in% covariateIdsChunk) |>
          dplyr::distinct(rowId, covariateId, covariateValue) |>
          dplyr::collect() |>
          dplyr::anti_join(covarCase, by = c("rowId" = "personId"))
      ) |>
        dplyr::distinct() |>
        tidyr::spread(covariateId, covariateValue)

      covariatesWideTable <- caseControlCovariateWide |>
        dplyr::left_join(covariatesWideTable, by = c("personId" = "rowId")) |>
        dplyr::mutate(dplyr::across(BinaryCovariateNames, ~ dplyr::if_else(is.na(.), FALSE, TRUE))) |>
        as.data.frame()

      # calculate using speedglm
      outcomes <- covariatesWideTable |>
        dplyr::select(3:ncol(covariatesWideTable)) |>
        colnames()
      predictors <- covariatesWideTable |>
        dplyr::select(2) |>
        colnames()
      covariates <- as.character(covariatesIds)
      outcomes <- outcomes |> dplyr::setdiff(covariates)

      ParallelLogger::logInfo("Running ", length(outcomes), " regresions for ", nrow(covariatesWideTable), " subjects, and ", length(covariates), " covariates")
      ParallelLogger::logInfo("Using ", cores, " cores")

      cluster <- ParallelLogger::makeCluster(numberOfThreads = cores)

      # x
      if (cores == 1) {
        x <- list(outcomes)
      } else {
        x <- split(outcomes, cut(seq_along(outcomes), breaks = cores, labels = FALSE))
      }

      # fun
      .fun <- function(outcomes, predictors, covariates, data) {
        results <- tibble::tibble()
        for (outcome in outcomes) {
          formula <- as.formula(paste(paste0("`", outcome, "`"), " ~ ", paste(paste0("`", c(predictors, covariates), "`"), collapse = " + ")))
          family <- if (data[[outcome]] |> is.logical()) {
            binomial()
          } else {
            gaussian()
          }
          model <- NULL
          error <- ""
          tryCatch(
            {
              model <- speedglm::speedglm(formula, data, family)
            },
            error = function(e) {
              error <<- paste0("[Error in speedglm: ", e$message, "]")
            }
          )

          # If the models did not converge or error, report NA values instead.
          or <- NA
          p <- NA
          note <- ""
          if (is.null(model)) {
            note <- error
          } else {
            modsum <- summary(model)
            if (model$convergence) {
              gen_list <- grep(predictors, row.names(modsum$coefficients))
              or <- exp(modsum$coefficients[gen_list, 1])
              p <- modsum$coefficients[gen_list, 4]
            } else {
              note <- paste(note, "[Error: The model did not converge]")
            }
          }

          if (family$family == "binomial") {
            # n is the number of TRUE
            covariateType <- "binary"
            nCasesYes <- sum(data[[outcome]][data$caseControl == TRUE], na.rm = TRUE)
            nControlsYes <- sum(data[[outcome]][data$caseControl == FALSE], na.rm = TRUE)
            modelType <- "Logistic regresion"
          } else {
            # n is the number of no na
            covariateType <- "continuous"
            nCasesYes <- sum(!is.na(data[[outcome]][data$caseControl == TRUE]))
            nControlsYes <- sum(!is.na(data[[outcome]][data$caseControl == FALSE]))
            modelType <- "Linear Regresion"
          }

          results <- dplyr::bind_rows(results, tibble::tibble(
            covariateId = outcome,
            covariateType = covariateType,
            nCasesYes = nCasesYes,
            meanCases = mean(data[[outcome]][data$caseControl == TRUE], na.rm = TRUE),
            sdCases = sd(data[[outcome]][data$caseControl == TRUE], na.rm = TRUE),
            nControlsYes = nControlsYes,
            meanControls = mean(data[[outcome]][data$caseControl == FALSE], na.rm = TRUE),
            sdControls = sd(data[[outcome]][data$caseControl == FALSE], na.rm = TRUE),
            pValue = p,
            oddsRatio = or,
            modelType = modelType,
            runNotes = note
          ))
        }
        return(results)
      }

      parallelResults <- ParallelLogger::clusterApply(cluster, x, .fun, predictors, covariates, covariatesWideTable)
      chunkResults <- dplyr::bind_rows(parallelResults)
      ParallelLogger::stopCluster(cluster)

      codewasResults <- dplyr::bind_rows(codewasResults, chunkResults)

      analysisRef <- dplyr::bind_rows(
        covariate_case$analysisRef |> tibble::as_tibble(),
        covariate_control$analysisRef |> tibble::as_tibble()
      ) |>
        dplyr::distinct()

      covariateRef <- dplyr::bind_rows(
        covariate_case$covariateRef |> tibble::as_tibble(),
        covariate_control$covariateRef |> tibble::as_tibble()
      ) |>
        dplyr::distinct()
    }
  }

  # Add And fix columns in covariateRef
  conceptIds <- covariateRef |>
    dplyr::select(conceptId) |>
    dplyr::collect() |>
    dplyr::distinct() |>
    dplyr::rename(concept_id = conceptId)

  DatabaseConnector::insertTable(
    connection = connection,
    tableName = "temp_concept_ids",
    data = conceptIds,
    tempTable = TRUE,
    dropTableIfExists = TRUE
  )

  sql <- "
  SELECT c.concept_id, c.concept_code, c.vocabulary_id, c.standard_concept
  FROM @vocabulary_database_schema.concept AS c
  INNER JOIN #temp_concept_ids AS tci ON c.concept_id = tci.concept_id
  "
  conceptIdsAndCodes  <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    vocabulary_database_schema = vocabularyDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  ) |>
    tibble::as_tibble()

  covariateRef <- covariateRef |>
    dplyr::left_join(conceptIdsAndCodes, by = "conceptId")

  if (length(regexAnalysisIds) > 0 && !is.null(cohortDefinitionTable)) {
    conceptIdsAndCohortDescriptions <- cohortDefinitionTable |>
      dplyr::cross_join(analysisRegexTibble) |>
      dplyr::filter(stringr::str_detect(cohort_definition_name, analysisRegex)) |>
      dplyr::mutate(covariateId = cohort_definition_id * 1000 + analysisId) |>
      dplyr::transmute(
        covariateId = as.integer(covariateId),
        conceptCode2 = as.character(cohort_definition_name),
        covariateName2 = as.character(cohort_definition_description),
        vocabularyId2 = analysisName
      )

    covariateRef <- covariateRef |>
      dplyr::left_join(conceptIdsAndCohortDescriptions, by = "covariateId") |>
      dplyr::mutate(
        conceptCode = dplyr::if_else(!is.na(conceptCode2), conceptCode2, conceptCode),
        covariateName = dplyr::if_else(!is.na(covariateName2), covariateName2, covariateName),
        vocabularyId = dplyr::if_else(!is.na(vocabularyId2), vocabularyId2, vocabularyId)
      ) |>
      dplyr::select(-conceptCode2, -covariateName2, -vocabularyId2)
  }

  ParallelLogger::logInfo("CohortDiagnostics_runCodeWAS completed")
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

  # CodeWASCounts ------------------------------------------------
  codewasResults <- codewasResults |>
    dplyr::transmute(
      databaseId = as.character({{ databaseId }}),
      covariateId = as.double(covariateId),
      covariateType = as.character(covariateType),
      nCasesYes = as.integer(nCasesYes),
      meanCases = as.double(meanCases),
      sdCases = as.double(sdCases),
      nControlsYes = as.integer(nControlsYes),
      meanControls = as.double(meanControls),
      sdControls = as.double(sdControls),
      pValue = as.double(pValue),
      oddsRatio = as.double(oddsRatio),
      modelType = as.character(modelType),
      runNotes = as.character(runNotes)
    )
  duckdb::dbWriteTable(connection, "codewasResults", codewasResults, overwrite = TRUE)

  if (!is.null(analysisRegexTibble)) {
    analysisRef <- analysisRef |>
      dplyr::left_join(
        analysisRegexTibble |> dplyr::rename(newAnalysisName = analysisName),
        by = "analysisId"
      ) |>
      dplyr::mutate(analysisName = dplyr::if_else(is.na(newAnalysisName), analysisName, newAnalysisName))
  }

  analysisRef <- analysisRef |>
    dplyr::transmute(
      analysisId = as.double(analysisId),
      analysisName = as.character(analysisName),
      domainId = dplyr::if_else(domainId == "cohort", "Cohort", domainId),
      isBinary = as.character(isBinary),
      missingMeansZero = as.character(missingMeansZero)
    )
  duckdb::dbWriteTable(connection, "analysisRef", analysisRef, overwrite = TRUE)

  # TEMP
  # If missing valueAsConceptId or collisions, add them as NA
  if (!"valueAsConceptId" %in% names(covariateRef)) {
    covariateRef$valueAsConceptId <- NA
  }
  if (!"collisions" %in% names(covariateRef)) {
    covariateRef$collisions <- NA
  }
  # END TEMP

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

  # analysisInfo ------------------------------------------------
  exportDuration <- Sys.time() - startExportTime

  analysisInfo <- tibble::tibble(
    analysisType = "codeWAS",
    version = "1.1.0",
    analysisSettings = yaml::as.yaml(analysisSettings),
    analysisDuration = analysisDuration,
    exportDuration = exportDuration
  )
  duckdb::dbWriteTable(connection, "analysisInfo", analysisInfo, overwrite = TRUE)

  # close connection
  duckdb::dbDisconnect(connection)

  ParallelLogger::logInfo("Results exported")

  #
  # if cohortIdControls is 0 delete the match control cohort
  #
  if (length(cohortsToDelete) > 0) {
    ParallelLogger::logInfo("Deleting match control cohorts")

    cohortTableHandler$deleteCohorts(cohortsToDelete)

  }

  return(pathToResultsDatabase)
}

#' @title Assert Analysis Settings for CodeWAS
#' @description Validates the `analysisSettings` list to ensure it contains the required elements (`cohortIdCases`, `cohortIdControls`, `analysisIds`, `covariatesIds`, `minCellCount`, `chunksSizeNOutcomes`, `cores`) with correct types and values. This function is specifically designed for checking settings related to CodeWAS analysis.
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
assertAnalysisSettings_CodeWAS <- function(analysisSettings) {
  analysisSettings |> checkmate::assertList()
  c("cohortIdCases", "cohortIdControls", "analysisIds") |> checkmate::assertSubset(names(analysisSettings))
  analysisSettings$cohortIdCases |> checkmate::assertNumeric(lower = 1, len = 1)
  analysisSettings$cohortIdControls |> checkmate::assertNumeric(lower = 0, len = 1)
  analysisSettings$analysisIds |> checkmate::assertNumeric()

  if (is.null(analysisSettings$covariatesIds)) {
    analysisSettings$covariatesIds <- NULL
  }
  analysisSettings$covariatesIds |> checkmate::assertNumeric(null.ok = TRUE)

  if (is.null(analysisSettings$minCellCount)) {
    analysisSettings$minCellCount <- 1
  }
  analysisSettings$minCellCount |> checkmate::assertNumeric()

  if (is.null(analysisSettings$chunksSizeNOutcomes)) {
    analysisSettings$chunksSizeNOutcomes <- 1000000000000
  }
  analysisSettings$chunksSizeNOutcomes |> checkmate::assertNumeric(null.ok = TRUE)

  if (is.null(analysisSettings$cores)) {
    analysisSettings$cores <- 1
  }
  analysisSettings$cores |> checkmate::assertNumeric()

  if (is.null(analysisSettings$analysisRegex)) {
    analysisSettings$analysisRegexTibble <- NULL
  } else {
    analysisSettings$analysisRegexTibble |> checkmate::assertTibble()
    analysisSettings$analysisRegexTibble |>
      names() |>
      checkmate::assertSetEqual(c("analysisId", "analysisName", "analysisRegex"))
  }

  return(analysisSettings)
}


#' @title Check CodeWAS Results
#' @description This function checks the integrity and correctness of the exported CodeWAS results in a DuckDB database.
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
checkResults_CodeWAS <- function(pathToResultsDatabase) {
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
