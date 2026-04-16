#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactive parseQueryString observe req insertUI onFlushed observeEvent
#' @importFrom duckdb dbConnect duckdb
#' @importFrom dplyr tbl collect pull
#' @importFrom ParallelLogger logInfo
#' @noRd
app_server <- function(input, output, session) {
  #
  # Retrieve path from the URL and copy them to
  #
  rf_urlParams <- shiny::reactive({
    query <- shiny::parseQueryString(session$clientData$url_search)
    analysisType <- query[["analysisType"]]
    pathToResultsDatabase <- query[["pathToResultsDatabase"]]
    if (!is.null(analysisType) && !is.null(pathToResultsDatabase)) {
      return(list(analysisType = analysisType, pathToResultsDatabase = pathToResultsDatabase))
    } else {
      return(list(analysisType = "", pathToResultsDatabase = ""))
    }
  })

  #
  # based on rf_pathToResultsDatabase, loads module ui. Then clicks hidenButton to trigger the server load
  #
  shiny::observe({
    shiny::req(rf_urlParams())

    # get parameters from url
    analysisTypeFromUrl <- rf_urlParams()$analysisType
    pathToResultsDatabaseFromUrl <- rf_urlParams()$pathToResultsDatabase

    # get parameters from options
    analysisTypeFromOptions <- getOption("CO2AnalysisModules.analysisType")
    pathToResultsDatabaseFromOptions <- getOption("CO2AnalysisModules.pathToResultsDatabase")
    pathToLogsFromOptions <- getOption("CO2AnalysisModules.pathToLogs")

    # if parameters empty or have change, the update and reload
    if (
      analysisTypeFromUrl != analysisTypeFromOptions || pathToResultsDatabaseFromUrl != pathToResultsDatabaseFromOptions
    ) {
      # log start
      timestamp <- as.character(as.numeric(format(Sys.time(), "%d%m%Y%H%M%OS2")) * 100)
      logsFolder <- paste0(analysisTypeFromOptions, "_", timestamp)
      logshref <- fcr_setUpLogger(logsFolder = logsFolder)

      # log start
      ParallelLogger::logInfo("[Start] Start logging")
      ParallelLogger::logInfo("[Start] analysisTypeFromUrl: ", analysisTypeFromUrl, ", pathToResultsDatabaseFromUrl: ", pathToResultsDatabaseFromUrl)
      ParallelLogger::logInfo("[Start] analysisTypeFromOptions: ", analysisTypeFromOptions, ", pathToResultsDatabaseFromOptions: ", pathToResultsDatabaseFromOptions)

      options("CO2AnalysisModules.analysisType" = analysisTypeFromUrl)
      options("CO2AnalysisModules.pathToResultsDatabase" = pathToResultsDatabaseFromUrl)
      options("CO2AnalysisModules.pathToLogs" = logshref)
      ParallelLogger::logInfo("[Start] Reload UI ")
      session$reload()
    } else {
      ParallelLogger::logInfo("[Start] analysisTypeFromUrl: ", analysisTypeFromUrl, ", pathToResultsDatabaseFromUrl: ", pathToResultsDatabaseFromUrl)
      ParallelLogger::logInfo("[Start] analysisTypeFromOptions: ", analysisTypeFromOptions, ", pathToResultsDatabaseFromOptions: ", pathToResultsDatabaseFromOptions)
      # if up to date call module server
      if (file.exists(pathToResultsDatabaseFromOptions) == TRUE) {


        # copy the data for download
        download_href <- NULL

        if (grepl("\\.duckdb$", pathToResultsDatabaseFromOptions, ignore.case = TRUE)) {
          analysisType <- getOption("CO2AnalysisModules.analysisType")
          sanitized <- gsub("[^[:alnum:]]+", "_", analysisType)
          sanitized <- gsub("^_+|_+$", "", sanitized)
          download_name <- paste0(sanitized, "_analysisResults.duckdb")

          download_dir <- file.path(tempdir(), paste0("viewer_download_", session$token))
          dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

          served_file <- file.path(download_dir, download_name)

          ok <- tryCatch(
            file.copy(pathToResultsDatabaseFromOptions, served_file, overwrite = TRUE),
            error = function(e) FALSE
          )

          if (isTRUE(ok) && file.exists(served_file) && file.info(served_file)$size > 0) {
            resource_prefix <- paste0("duckdbdownload_", session$token)
            shiny::addResourcePath(resource_prefix, download_dir)
            download_href <- paste0("/", resource_prefix, "/", download_name)
          } else {
            ParallelLogger::logWarn("[Start] Could not prepare DuckDB download file: ", pathToResultsDatabaseFromOptions)
          }
        }

        # read database
        # Try loading as Andromeda first
        analysisResults <- tryCatch(
          Andromeda::loadAndromeda(pathToResultsDatabaseFromOptions),
          error = function(e) NULL
        )

        # If not Andromeda, open as DuckDB
        if (is.null(analysisResults)) {
          analysisResults <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabaseFromOptions)
        }

        # load module server based on analysisType
        if (analysisTypeFromOptions == "cohortOverlaps") {
          mod_resultsVisualisation_server("cohortOverlaps", mod_resultsVisualisation_CohortsOverlaps_server, analysisResults, "Cohort Overlaps", pathToLogsFromOptions, download_href)
        }
        if (analysisTypeFromOptions == "cohortDemographics") {
          mod_resultsVisualisation_server("cohortDemographics", mod_resultsVisualisation_CohortsDemographics_server, analysisResults,"Cohort Demographics", pathToLogsFromOptions, download_href)
        }
        if (analysisTypeFromOptions == "codeWAS") {
          mod_resultsVisualisation_server("codeWAS", mod_resultsVisualisation_CodeWAS_server, analysisResults, "CodeWAS", pathToLogsFromOptions, download_href)
        }
        if (analysisTypeFromOptions == "timeCodeWAS") {
          mod_resultsVisualisation_server("timeCodeWAS", mod_resultsVisualisation_TimeCodeWAS_server, analysisResults, "TimeCodeWAS", pathToLogsFromOptions, download_href)
        }
        if (analysisTypeFromOptions == "phenotypeScoring") {
          mod_resultsVisualisation_server("phenotypeScoring", mod_resultsVisualisation_PhenotypeScoring_server, analysisResults, "Phenotype Scoring", pathToLogsFromOptions, download_href)
        }
      }
    }

    ParallelLogger::logInfo("[Start] Loaded module server for ", analysisTypeFromOptions)
  })

}
