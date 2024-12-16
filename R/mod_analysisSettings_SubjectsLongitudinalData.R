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
    shinyFeedback::useShinyFeedback(),
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
      label = "Select controls cohort (not working yet):",
      choices = NULL,
      multiple = FALSE
    ),
    #
    shiny::tags$h4("Settings"),
    shiny::numericInput(
      inputId = ns("nSubjects_numericInput"),
      label = "Number of subjects to sample",
      value = 100,
      min = 1,
      max = 100
    ),
    shiny::numericInput(
      inputId = ns("seed_numericInput"),
      label = "Random seed",
      value = 123,
      min = 1
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
      if (nrow(cohortIdAndNames) != 0) {
        cohortIdAndNamesList <- as.list(setNames(
          cohortIdAndNames$cohortId,
          paste(cohortIdAndNames$shortName, "(", cohortIdAndNames$cohortName, ")")
        ))
      }

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "cohortIdCases_pickerInput",
        choices = cohortIdAndNamesList
      )
    })


    #
    # Shinyfeedback
    #
    shiny::observe({
      shiny::req(input$nSubjects_numericInput)
      shinyFeedback::feedbackWarning(
        inputId = "nSubjects_numericInput",
        show = input$nSubjects_numericInput > 100,
        text = "Number of subjects is too high, max is 100"
      )
    })

    #
    # activate settings if cases cohort has been selected
    #
    shiny::observe({
      condition <- !is.null(input$cohortIdCases_pickerInput)
      shinyjs::toggleState("nSubjects_numericInput", condition = condition)
      shinyjs::toggleState("seed_numericInput", condition = condition)
    })

    #
    # return reactive options
    #
    rf_analysisSettings <- shiny::reactive({
      if (is.null(input$cohortIdCases_pickerInput) ||
        is.null(input$nSubjects_numericInput) ||
        is.na(input$nSubjects_numericInput) ||
        is.null(input$seed_numericInput) ||
        is.na(input$seed_numericInput)) {
        return(NULL)
      }

      analysisSettings <- list(
        cohortIdCases = input$cohortIdCases_pickerInput |> as.integer(),
        cohortIdControls = if (length(input$cohortIdControls_pickerInput) == 0) NULL else input$cohortIdControls_pickerInput |> as.integer(),
        nSubjects = if(input$nSubjects_numericInput > 100) 100 else input$nSubjects_numericInput |> as.integer(),
        seed = input$seed_numericInput |> as.integer(),
        prevalenceTable = NULL
      )

      return(analysisSettings)
    })

    return(rf_analysisSettings)
  })
}
