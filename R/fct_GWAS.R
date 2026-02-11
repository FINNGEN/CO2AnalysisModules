
#' @title execute_GWAS
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
  casesCohort <- analysisSettings$cohortIdCases
  controlsCohort <- analysisSettings$cohortIdControls
  phenotype <- analysisSettings$phenotype
  description <- analysisSettings$description
  analysisType <- analysisSettings$analysisType
  release <- analysisSettings$release
  connectionSandboxAPI <- analysisSettings$connectionSandboxAPI

  #
  # get connectionSandboxAPI
  #
  ParallelLogger::logInfo("Get connectionSandboxAPI GWAS analysis")

  #
  # Send the GWAS analysis
  #
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
  ] |> unique()

  controlsFinngenids <- cohortData$person_source_value[
    which(cohortData$cohort_name == controlsCohort$cohortName)
  ] |> unique()

  ParallelLogger::logInfo("Running GWAS using:",
                          #"\nnotification_email: ", connectionSandboxAPI$notification_email,
                          "\nname: ", connectionSandboxAPI$name,
                          "\nsubmitted cases: ", length(casesFinngenids),
                          "\nsubmitted controls: ", length(controlsFinngenids),
                          "\nphenotype: ", phenotype,
                          "\ndescription: ", description,
                          "\nanalysisType: ", analysisType,
                          "\nrelease: ", release)

  # This was using the custom gwas api, which is being fazed out in favor of the standard pipeline to enable additional features such as
  # finemapping and association anaysis for HLA alleles

  # result <- FinnGenUtilsR::runGWASAnalysis(
  #   connection_sandboxAPI = connectionSandboxAPI,
  #   cases_finngenids = casesFinngenids,
  #   controls_finngenids = controlsFinngenids,
  #   phenotype_name = phenotype,
  #   title = phenotype,
  #   description = description,
  #   notification_email = connectionSandboxAPI$notification_email,
  #   analysis_type = analysisType,
  #   release = release
  # )
  #

  # the release is not needed now, we will need to change this based on the release and pipeline name to find the right
  # pipeline id

  # also later allow usesrs to add additional covariates via extra_covariates_df, and selection of finngen standard covariates from the covariates argument

  result <- FinnGenUtilsR::runRegenieStandardPipeline(
    connection_sandboxAPI = connectionSandboxAPI,
    cases_finngenids = casesFinngenids,
    controls_finngenids = controlsFinngenids,
    phenotype_name = phenotype,
    phenotype_description = description,
    test = analysisType
  )


  if (result$status == FALSE){
    ParallelLogger::logInfo("GWAS run completed: ", result)
    stop("GWAS run failed", result$message)

  }else{

    # the result contains more detail information, we can record and display that if the result is succesful
    ParallelLogger::logInfo("GWAS run completed: ", result)

  }

  # there is not database to share
  return(NULL)

}


#' @title Assert Analysis Settings for GWAS
#' @description Validates the `analysisSettings` list to ensure it contains the required elements
#' @param analysisSettings A list containing analysis settings. It must include the following elements:
#' \describe{
#'   \item{casesFinngenids}{A vector of strings containing cases FinnGen IDs.}
#'   \item{controlsFinngenids}{A vector of strings containing controls FinnGen IDs.}
#'   \item{analysisType}{String containing analysis type (additive, recessive, or dominant).}
#'   \item{phenotype}{String containing phenotype name.}
#'   \item{description}{String containing description for the custom GWAS analysis.}
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
  c('cohortIdCases', 'cohortIdControls', 'phenotype', 'description', 'analysisType', 'release', 'connectionSandboxAPI')  |> checkmate::assertSubset(names(analysisSettings))
  analysisSettings$cohortIdCases |> checkmate::assertNumeric()
  analysisSettings$cohortIdControls |> checkmate::assertNumeric()
  analysisSettings$phenotype |> checkmate::assertString(min.chars = 1, pattern = "^[A-Za-z][A-Za-z0-9_]*$")
  analysisSettings$description |> checkmate::assertString(min.chars = 1)
  analysisSettings$analysisType |> checkmate::assertChoice(choices = c("additive", "recessive", "dominant"))
  analysisSettings$release |> checkmate::assertChoice(choices = c("Regenie13", "Regenie12"))
  analysisSettings$connectionSandboxAPI  |> checkmate::assertList()
  return(analysisSettings)
}


