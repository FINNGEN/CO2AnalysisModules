
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
  casesCohort <- analysisSettings$cohortIdCases
  controlsCohort <- analysisSettings$cohortIdControls
  phenotype <- analysisSettings$phenotype
  description <- analysisSettings$description
  analysisType <- analysisSettings$analysisType
  release <- analysisSettings$release

  #
  # get connectionSandboxAPI
  #
  ParallelLogger::logInfo("Get connectionSandboxAPI GWAS analysis")

  # TEMP it may be that the token gets outdated
  token <- Sys.getenv('SANDBOX_TOKEN')
  if (is.null(token)  | token == ""){
    stop("SANDBOX_TOKEN is not set")
  }

  # update token
  # if different version of openssl package is used in docker and URL host
  # there will be an error. To avoid the error set up the following configs
  httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

  base_url <- "https://internal-api.app.finngen.fi/internal-api/"
  if(token != "test"){

    # refresh the token
    authorization <- paste("Bearer", token)
    headers <- httr::add_headers(c('Authorization' = authorization))
    url <- paste0(base_url, "v2/user/refresh-token")

    # fetch refreshed token
    error <- NULL
    tryCatch({
      res <- httr::GET(url, config = headers)
      if(res$status_code == 200){
        token <- jsonlite::fromJSON(rawToChar(res$content))$token
        Sys.setenv(SANDBOX_TOKEN = token)
      } else {
        error <<- paste("status code ", res$status_code)
      }
    }, error = function(e){
      error <<- e$message
    })

    # update token in the environment variable
    if(!is.null(error)){
      stop("Error refreshing token: ", error)
    }
  }

  connectionSandboxAPI <- FinnGenUtilsR::createSandboxAPIConnection(base_url, token)

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
                          "\nnotification_email: ", connectionSandboxAPI$notification_email,
                          "\nname: ", connectionSandboxAPI$name,
                          "\nsubmitted cases: ", length(casesFinngenids),
                          "\nsubmitted controls: ", length(controlsFinngenids),
                          "\nphenotype: ", phenotype,
                          "\ndescription: ", description,
                          "\nanalysisType: ", analysisType,
                          "\nrelease: ", release)

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

  ParallelLogger::logInfo("GWAS run completed: ", result)

  if (!result$status){
    stop("GWAS run failed", result$message)
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
  c('cohortIdCases', 'cohortIdControls', 'phenotype', 'description', 'analysisType', 'release')  |> checkmate::assertSubset(names(analysisSettings))
  analysisSettings$cohortIdCases |> checkmate::assertNumeric()
  analysisSettings$cohortIdControls |> checkmate::assertNumeric()
  analysisSettings$phenotype |> checkmate::assertString(min.chars = 1, pattern = "^[A-Z0-9]+$")
  analysisSettings$description |> checkmate::assertString(min.chars = 1)
  analysisSettings$analysisType |> checkmate::assertChoice(choices = c("additive", "recessive", "dominant"))
  analysisSettings$release |> checkmate::assertChoice(choices = c("Regenie12"))
  return(analysisSettings)
}


