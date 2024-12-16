#' @title Subjects Longitudinal Data Analysis Settings UI
#' @description UI module for configuring the settings for subjects longitudinal data analysis.
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
mod_analysisSettings_subjectsLongitudinalData_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Cohorts"),
    shinyWidgets::pickerInput(
      inputId = ns("cohortIdCases_pickerInput"),
      label = "Select cases cohort:",
      choices = NULL,
      multiple = FALSE
    ),
    shinyWidgets::pickerInput(
      inputId = ns("cohortIdControls_pickerInput"),
      label = "Select controls cohort (optional):",
      choices = NULL,
      multiple = FALSE
    ),
    #
    shiny::tags$h4("Settings"),
    shiny::numericInput(
      inputId = ns("nSubjects_numericInput"),
      label = "Number of subjects to sample",
      value = 100,
      min = 1
    ),
    shiny::numericInput(
      inputId = ns("seed_numericInput"),
      label = "Random seed",
      value = 123,
      min = 1
    ),
    shinyWidgets::pickerInput(
      inputId = ns("prevalenceTable_pickerInput"),
      label = "Prevalence table (optional):",
      choices = NULL,
      multiple = FALSE
    )
  )
}

#' @title Subjects Longitudinal Data Analysis Settings Server
#' @description Server module for handling the logic of the subjects longitudinal data analysis settings UI.
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
mod_analysisSettings_subjectsLongitudinalData_server <- function(id, r_connectionHandler) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # update cohort pickers with cohort names in r_connectionHandler
    #
    shiny::observe({
      shiny::req(r_connectionHandler$cohortTableHandler)
      shiny::req(r_connectionHandler$hasChangeCounter)

      cohortIdAndNames <- r_connectionHandler$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- list()
      if(nrow(cohortIdAndNames) != 0){
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, 
          paste(cohortIdAndNames$shortName, "("  , cohortIdAndNames$cohortName, ")")))
      }

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "cohortIdCases_pickerInput",
        choices = cohortIdAndNamesList
      )

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "cohortIdControls_pickerInput",
        choices = c("None" = "", cohortIdAndNamesList)
      )
    })

    #
    # update prevalence table picker
    #
    shiny::observe({
      shiny::req(r_connectionHandler$connectionHandler)
      
      # Get list of tables from database
      tables <- r_connectionHandler$connectionHandler$getTableNames()
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "prevalenceTable_pickerInput",
        choices = c("None" = "", tables)
      )
    })

    #
    # activate settings if cases cohort has been selected
    #
    shiny::observe({
      condition <- !is.null(input$cohortIdCases_pickerInput)
      shinyjs::toggleState("nSubjects_numericInput", condition = condition)
      shinyjs::toggleState("seed_numericInput", condition = condition)
      shinyjs::toggleState("prevalenceTable_pickerInput", condition = condition)
    })

    #
    # return reactive options
    #
    rf_analysisSettings <- shiny::reactive({
      if(is.null(input$cohortIdCases_pickerInput)){
        return(NULL)
      }

      analysisSettings <- list(
        cohortIdCases = input$cohortIdCases_pickerInput |> as.integer(),
        cohortIdControls = if(input$cohortIdControls_pickerInput == "") NULL else as.integer(input$cohortIdControls_pickerInput),
        nSubjects = input$nSubjects_numericInput,
        seed = input$seed_numericInput,
        prevalenceTable = if(input$prevalenceTable_pickerInput == "") NULL else input$prevalenceTable_pickerInput
      )

      return(analysisSettings)
    })

    return(rf_analysisSettings)
  })
}