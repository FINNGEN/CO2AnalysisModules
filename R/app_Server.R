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
        # read database
        analysisResults <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabaseFromOptions)

        # load module server based on analysisType
        if (analysisTypeFromOptions == "cohortOverlaps") {
          mod_resultsVisualisation_server("cohortOverlaps", mod_resultsVisualisation_CohortsOverlaps_server, analysisResults)
        }
        if (analysisTypeFromOptions == "cohortDemographics") {
          mod_resultsVisualisation_server("cohortDemographics", mod_resultsVisualisation_CohortsDemographics_server, analysisResults)
        }
        if (analysisTypeFromOptions == "codeWAS") {
          mod_resultsVisualisation_server("codeWAS", mod_resultsVisualisation_CodeWAS_server, analysisResults)
        }
        if (analysisTypeFromOptions == "timeCodeWAS") {
          mod_resultsVisualisation_server("timeCodeWAS", mod_resultsVisualisation_TimeCodeWAS_server, analysisResults)
        }
        if (analysisTypeFromOptions == "phenotypeScoring") {
          mod_resultsVisualisation_server("phenotypeScoring", mod_resultsVisualisation_PhenotypeScoring_server, analysisResults)
        }
      }
    }

    ParallelLogger::logInfo("[Start] Loaded module server for ", analysisTypeFromOptions)
  })

}
