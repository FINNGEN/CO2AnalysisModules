#' Launch the Shiny Application
#'
#' This function launches a Shiny application for CO2 analysis, configured with the provided modules.
#'
#' @param CO2AnalysisModulesConfig A list containing the configuration for the CO2 analysis modules.
#' @param ... Additional arguments passed to `shiny::shinyApp`.
#'
#' @importFrom checkmate assert_list
#' @importFrom shiny shinyApp
#'
#' @return A Shiny application object.
#' @export
run_app <- function(CO2AnalysisModulesConfig, ...) {

  CO2AnalysisModulesConfig |> checkmate::assert_list()

  app  <- shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    ...
  )

  # setup shiny options
  app$appOptions$CO2AnalysisModulesConfig  <- CO2AnalysisModulesConfig

  return(app)
}

