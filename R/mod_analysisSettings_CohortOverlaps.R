#' @title Cohort Overlaps Analysis Settings UI
#' @description UI module for configuring the settings for cohort overlaps analysis. This module allows users to select cohorts and set the minimum cell count for the analysis.
#'
#' @param id A string representing the module's namespace.
#'
#' @return A Shiny UI element that can be included in a Shiny app.
#'
#' @importFrom shiny NS tags h4 numericInput
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tagList
#'
#' @export
#'
mod_analysisSettings_cohortOverlaps_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Cohorts"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCohorts_pickerInput"),
      label = "Select one or more cohorts:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE),
    shiny::tags$h4("Analysis Settings"),
    shiny::numericInput(
      inputId = ns("minCellCount_numericInput"),
      label = "Minimum cell count:",
      value = 1,
      min = 1,
      max = 1000
    )
  )
}

#' @title Cohort Overlaps Analysis Settings Server
#' @description Server module for handling the logic of the cohort overlaps analysis settings UI. This module updates the UI elements based on the selected cohorts and returns the analysis settings as a reactive expression.
#'
#' @param id A string representing the module's namespace.
#' @param r_connectionHandler A reactive object that provides access to the cohort data and connection information.
#'
#' @return A reactive expression that returns the analysis settings as a list.
#'
#' @importFrom shiny moduleServer reactive req observe
#' @importFrom shinyjs toggleState
#' @importFrom shinyWidgets updatePickerInput
#'
#' @export
#'
mod_analysisSettings_cohortOverlaps_server <- function(id, r_connectionHandler) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #

    #
    # update selectCohorts_pickerInput with cohort names in r_connectionHandler
    #
    shiny::observe({
      shiny::req(r_connectionHandler$cohortTableHandler)
      shiny::req(r_connectionHandler$hasChangeCounter)

      cohortIdAndNames <- r_connectionHandler$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- list()
      if(nrow(cohortIdAndNames) != 0){
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "("  , cohortIdAndNames$cohortName, ")")))
      }

      shinyWidgets::updatePickerInput(
        inputId = "selectCohorts_pickerInput",
        choices = cohortIdAndNamesList,
        selected = cohortIdAndNamesList
      )
    })

    #
    # activate settings if cohorts have been selected
    #
    shiny::observe({
      condition <- !is.null(input$selectCohorts_pickerInput)
      shinyjs::toggleState("minCellCount_numericInput", condition = condition )
    })


    #
    # return reactive options
    #
    rf_analysisSettings <- shiny::reactive({
      if(is.null(input$selectCohorts_pickerInput)){
        return(NULL)
      }
      analysisSettings <- list(
        cohortIds = input$selectCohorts_pickerInput |> as.integer(),
        minCellCount = input$minCellCount_numericInput
      )

      return(analysisSettings)

    })

    return(rf_analysisSettings)

  })
}





















