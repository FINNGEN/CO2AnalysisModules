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
  # Retrieve path from the URL and copy them to r$pathToResultsDatabase
  #
  rf_pathToResultsDatabase <- shiny::reactive({
    query <- shiny::parseQueryString(session$clientData$url_search)
    value <- query[['pathToResultsDatabase']]
    if (!is.null(value)) {
      return(value)
    }
  })

  #
  # based on rf_pathToResultsDatabase, loads module ui. Then clicks hidenButton to trigger the server load
  #
  shiny::observe({
    shiny::req(rf_pathToResultsDatabase())

    # read database
    analysisResults <- duckdb::dbConnect(duckdb::duckdb(), rf_pathToResultsDatabase())
    analysisType <- analysisResults |> dplyr::tbl("analysisInfo") |> dplyr::collect() |> dplyr::pull("analysisType") |> unique()

    timestamp  <- as.character(as.numeric(format(Sys.time(), "%d%m%Y%H%M%OS2"))*100)
    logsFolder <- paste0(analysisType, "_", timestamp)
    logshref  <- fcr_setUpLogger(logsFolder = logsFolder)
    # log start
    ParallelLogger::logInfo("[Start] Start logging")
    ParallelLogger::logInfo("[Start] pathToResultsDatabase: ", rf_pathToResultsDatabase())

    # select module ui based on analysisType
    if (analysisType == "cohortOverlaps") {
      pathAboutModule <- system.file('modulesDocumentation/about_cohortOverlaps.md', package = "CO2AnalysisModules")
      ui <- mod_resultsVisualisation_ui("cohortOverlaps", mod_resultsVisualisation_CohortsOverlaps_ui, pathAboutModule, "Cohort Overlaps", logshref)
    }
    if (analysisType == "cohortDemographics") {
      pathAboutModule <- system.file('modulesDocumentation/about_cohortDemographics.md', package = "CO2AnalysisModules")
      ui <- mod_resultsVisualisation_ui("cohortDemographics", mod_resultsVisualisation_CohortsDemographics_ui, pathAboutModule, "Cohort Demographics", logshref)
    }
    if (analysisType == "codeWAS") {
      pathAboutModule <- system.file('modulesDocumentation/about_codeWAS.md', package = "CO2AnalysisModules")
      ui <- mod_resultsVisualisation_ui("codeWAS", mod_resultsVisualisation_CodeWAS_ui, pathAboutModule, "CodeWAS", logshref)
    }
    if (analysisType == "timeCodeWAS") {
      pathAboutModule <- system.file('modulesDocumentation/about_timeCodeWAS.md', package = "CO2AnalysisModules")
      ui <- mod_resultsVisualisation_ui("timeCodeWAS", mod_resultsVisualisation_TimeCodeWAS_ui, pathAboutModule, "TimeCodeWAS", logshref)
    }


    # load module ui
    shiny::insertUI(
      selector = "#hidenButton",
      where = "afterEnd",
      ui = ui
    )

    ParallelLogger::logInfo("[Start] Loaded module UI for ", analysisType)

    # trigger button on flushed
    shiny::onFlushed(function (){
      shinyjs::runjs('$(".wrapper").css("height", "auto");')
      shinyjs::runjs('$(".shiny-spinner-placeholder").hide();')
      shinyjs::runjs('$(".load-container.shiny-spinner-hidden.load1").hide();')
      shinyjs::runjs('$("#hidenButton").click();')
    })

  })

  #
  # when the button is clicked after flushed, load the module server,  rf_pathToResultsDatabase
  #
  shiny::observeEvent(input$hidenButton,{
    shiny::req(rf_pathToResultsDatabase())

    # read database
    analysisResults <- duckdb::dbConnect(duckdb::duckdb(), rf_pathToResultsDatabase())
    analysisType <- analysisResults |> dplyr::tbl("analysisInfo") |> dplyr::collect() |> dplyr::pull("analysisType") |> unique()

    # load module server based on analysisType
    if(analysisType == "cohortOverlaps"){
      mod_resultsVisualisation_server("cohortOverlaps", mod_resultsVisualisation_CohortsOverlaps_server, analysisResults)
    }
    if(analysisType == "cohortDemographics"){
      mod_resultsVisualisation_server("cohortDemographics", mod_resultsVisualisation_CohortsDemographics_server, analysisResults)
    }
    if(analysisType == "codeWAS"){
      mod_resultsVisualisation_server("codeWAS", mod_resultsVisualisation_CodeWAS_server, analysisResults)
    }
    if(analysisType == "timeCodeWAS"){
      mod_resultsVisualisation_server("timeCodeWAS", mod_resultsVisualisation_TimeCodeWAS_server, analysisResults)
    }

    ParallelLogger::logInfo("[Start] Loaded module server for ", analysisType)

  })

}
























