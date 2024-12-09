#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tagList tags div p
#'
#' @noRd
app_ui <- function(request) {
  # get options from url
  analysisTypeFromOptions <- getOption("CO2AnalysisModules.analysisType")
  pathToResultsDatabaseFromOptions <- getOption("CO2AnalysisModules.pathToResultsDatabase")
  pathToLogsFromOptions <- getOption("CO2AnalysisModules.pathToLogs")

  # if not options it means that the server did not receive the parameters
  if (analysisTypeFromOptions == "" || pathToResultsDatabaseFromOptions == "") {
    return(
      shiny::tagList(
        shiny::tags$div(
          shiny::tags$p("Loading ...")
        )
      )
    )
  } else {
    if (file.exists(pathToResultsDatabaseFromOptions) == FALSE) {
      return(
        shiny::tagList(
          shiny::tags$div(
            shiny::tags$p("The path to the results database does not exist: ", pathToResultsDatabaseFromOptions)
          )
        )
      )
    }

    # select module ui based on analysisType
    if (analysisTypeFromOptions == "cohortOverlaps") {
      pathAboutModule <- system.file("modulesDocumentation/about_cohortOverlaps.md", package = "CO2AnalysisModules")
      ui <- mod_resultsVisualisation_ui("cohortOverlaps", mod_resultsVisualisation_CohortsOverlaps_ui, pathAboutModule, "Cohort Overlaps", pathToLogsFromOptions)
      return(ui)
    }
    if (analysisTypeFromOptions == "cohortDemographics") {
      pathAboutModule <- system.file("modulesDocumentation/about_cohortDemographics.md", package = "CO2AnalysisModules")
      ui <- mod_resultsVisualisation_ui("cohortDemographics", mod_resultsVisualisation_CohortsDemographics_ui, pathAboutModule, "Cohort Demographics", pathToLogsFromOptions)
      return(ui)
    }
    if (analysisTypeFromOptions == "codeWAS") {
      pathAboutModule <- system.file("modulesDocumentation/about_codeWAS.md", package = "CO2AnalysisModules")
      ui <- mod_resultsVisualisation_ui("codeWAS", mod_resultsVisualisation_CodeWAS_ui, pathAboutModule, "CodeWAS", pathToLogsFromOptions)
      return(ui)
    }
    if (analysisTypeFromOptions == "timeCodeWAS") {
      pathAboutModule <- system.file("modulesDocumentation/about_timeCodeWAS.md", package = "CO2AnalysisModules")
      ui <- mod_resultsVisualisation_ui("timeCodeWAS", mod_resultsVisualisation_TimeCodeWAS_ui, pathAboutModule, "TimeCodeWAS", pathToLogsFromOptions)
      return(ui)
    }


    return(
      shiny::tagList(
        shiny::tags$div(
          shiny::tags$p("The analysis type is not supported: ", analysisTypeFromOptions)
        )
      )
    )
  }
}
