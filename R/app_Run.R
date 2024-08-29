#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(pathToCO2AnalysisModulesConfigYalm, ...) {

  # set up configuration
  checkmate::assertFileExists(pathToCO2AnalysisModulesConfigYalm, extension = "yml")
  CO2AnalysisModulesConfig <- yaml::read_yaml(pathToCO2AnalysisModulesConfigYalm)

  # set options
  # options(shiny.maxRequestSize = 314572800)
  # solves error in CohortDiagnostics (not used here)
  # options(java.parameters = "-Xss3m")

  # EXPLAIN WHY
  options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))


  app  <- shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    ...
  )

  # setup shiny options
  app$appOptions$CO2AnalysisModulesConfig  <- CO2AnalysisModulesConfig



  return(app)
}

