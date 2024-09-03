#' @title Cohort Demographics Analysis Settings UI
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
mod_analysisSettings_cohortDemographics_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Cohorts"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCohorts_pickerInput"),
      label = "Select one or more cohorts:",
      choices = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    #
    shiny::tags$h4("Settings"),
    shinyWidgets::pickerInput(
      inputId = ns("groupBy_pickerInput"),
      label = "Select counts stratification:",
      choices = list(`Calendar Year`='calendarYear', `Age Group`='ageGroup', `Gender`="gender"),
      selected = list(`Calendar Year`='calendarYear', `Age Group`='ageGroup', `Gender`="gender"),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    shinyWidgets::pickerInput(
      inputId = ns("referenceYears_pickerInput"),
      label = "Select counts refered to:",
      choices = list(`Cohort Onset`="cohort_start_date", `Cohort End`="cohort_end_date", `Birth`="birth_datetime"),
      selected = list(`Cohort Onset`="cohort_start_date", `Cohort End`="cohort_end_date", `Birth`="birth_datetime"),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    shiny::numericInput(
      inputId = ns("minCellCount_numericInput"),
      label = "Min Cell Count",
      value = 1,
      min = 1,
      max = 1000
    )
  )
}

#' @title Cohort Demographics Analysis Settings Server
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
mod_analysisSettings_cohortDemographics_server <- function(id, r_connectionHandler) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #

    #
    # update selectCohorts_pickerInput with cohort names in r_connectionHandler
    #
    shiny::observe({
      shiny::req(r_connectionHandler$hasChangeCounter)

      cohortIdAndNames <- r_connectionHandler$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, cohortIdAndNames$cohortName))

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
      shinyjs::toggleState("groupBy_pickerInput", condition = condition )
      shinyjs::toggleState("referenceYears_pickerInput", condition = condition )
      shinyjs::toggleState("minCellCount_numericInput", condition = condition )
    })

    #
    # return reactive options
    #
    rf_analysisSettings <- shiny::reactive({
      if(
        is.null(input$selectCohorts_pickerInput) |
        is.null(input$referenceYears_pickerInput) |
        is.null(input$groupBy_pickerInput) |
        is.null(input$minCellCount_numericInput)
      ){
        return(NULL)
      }

      analysisSettings <- list(
        cohortIds = input$selectCohorts_pickerInput |> as.integer(),
        referenceYears = input$referenceYears_pickerInput,
        groupBy = input$groupBy_pickerInput,
        minCellCount = input$minCellCount_numericInput
      )

      return(analysisSettings)

    })

    return(rf_analysisSettings)

  })
}





















