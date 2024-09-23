
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
#' @importFrom tibble tibble
#' @importFrom yaml as.yaml
#'
#' @export
#'
execute_GWAS <- function(
    exportFolder,
    cohortTableHandler,
    analysisSettings
) {
  #
  # Check parameters
  #
  exportFolder |> checkmate::assertDirectoryExists()
  cohortTableHandler |> checkmate::assertR6(class = "CohortTableHandler")
  analysisSettings <- analysisSettings |> assertAnalysisSettings_GWAS()

  # get parameters from analysisSettings
  phenotype <- analysisSettings$phenotype
  description <- analysisSettings$description
  analysisType <- analysisSettings$analysisType
  casesFinngenids <- analysisSettings$casesFinngenids
  controlsFinngenids <- analysisSettings$controlsFinngenids
  connectionSandboxAPI <- analysisSettings$connectionSandboxAPI
  release <- analysisSettings$release
  casesCohort <- analysisSettings$casesCohort
  controlsCohort <- analysisSettings$controlsCohort

  ParallelLogger::logInfo("Attempting to submit GWAS analysis")

  cohorts  <- cohortTableHandler$getCohortsSummary()
  casesCohort <- cohorts[cohorts$cohortId == casesCohort, ]
  controlsCohort <- cohorts[cohorts$cohortId == controlsCohort, ]

  cohortData <- HadesExtras::getCohortDataFromCohortTable(
    connection = cohortTableHandler$connectionHandler$getConnection(),
    cdmDatabaseSchema = cohortTableHandler$cdmDatabaseSchema,
    cohortDatabaseSchema = cohortTableHandler$cohortDatabaseSchema,
    cohortTable = cohortTableHandler$cohortTableNames$cohortTable,
    cohortNameIds = tibble::tibble(
      cohortId = c(casesCohort$cohortId, controlsCohort$cohortId),
      cohortName = c(casesCohort$cohortName, controlsCohort$cohortName))
  )

  casesFinngenids <- cohortData$person_source_value[
    which(cohortData$cohort_name == casesCohort$cohortName)
  ]

  controlsFinngenids <- cohortData$person_source_value[
    which(cohortData$cohort_name == controlsCohort$cohortName)
  ]

  tryCatch({
    result <- FinnGenUtilsR::runGWASAnalysis(
      connection_sandboxAPI = connectionSandboxAPI,
      cases_finngenids = casesFinngenids,
      controls_finngenids = controlsFinngenids,
      phenotype_name = phenotype,
      title = phenotype,
      description = description,
      notification_email = connectionSandboxAPI$notification_email,
      analysis_type = analysisType,
      release = release
    )
  }, error = function(e){
    ParallelLogger::logError("Error in FinnGenUtilsR::runGWASAnalysis:", e$message)
  })

  if (result$status){
    ParallelLogger::logInfo("GWAS run successfully submitted")
  } else {
    ParallelLogger::logError("Error submitting GWAS run", result$content)
  }

  return(result)

}

#' @title Assert Analysis Settings for GWAS
#' @description Validates the `analysisSettings` list to ensure it contains the required elements
#' @param analysisSettings A list containing analysis settings. It must include the following elements:
#' \describe{
#'   \item{casesFinngenids}{A vector of strings containing cases FinnGen IDs.}
#'   \item{controlsFinngenids}{A vector of strings containing controls FinnGen IDs.}
#'   \item{analysisType}{String containing analysis type (additive, recessive, or dominant).}
#'   \item{phenotype}{String containing phenotype name.}
#'   \item{description}{String containing descripition for the custom GWAS analysis.}
#'
#' }
#'
#' @return Returns the validated `analysisSettings` list.
#'
#' @importFrom checkmate assertList assertSubset assertNumeric
#'
#' @export
#'
assertAnalysisSettings_GWAS <- function(analysisSettings) {
  analysisSettings |> checkmate::assertList()
  c('casesFinngenids', 'controlsFinngenids', 'analysisType', 'phenotype', 'description', 'casesCohort', 'controlsCohort')  |> checkmate::assertSubset(names(analysisSettings))
  analysisSettings$analysisType |> checkmate::assert_choice(choices = c("additive", "recessive", "dominant"))
  analysisSettings$casesCohort |> checkmate::assertNumeric()
  analysisSettings$controlsCohort |> checkmate::assertNumeric()
  return(analysisSettings)
}


