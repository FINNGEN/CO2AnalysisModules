#' operateCohorts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS fileInput actionButton
#' @importFrom htmltools tagList hr
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
#' @importFrom shinyFeedback useShinyFeedback
mod_analysisSettings_GWAS_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    shiny::tags$h4("Cohorts"),
    shiny::tags$h5("Select case cohort:"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCaseCohort_pickerInput"),
      label = NULL,
      choices = NULL,
      selected = NULL,
      options = list(
        `actions-box` = TRUE),
      multiple = FALSE),
    shiny::tags$h5("Select control cohort:"),
    shinyWidgets::pickerInput(
      inputId = ns("selectControlCohort_pickerInput"),
      label = NULL,
      choices = NULL,
      selected = NULL,
      options = list(
        `actions-box` = TRUE),
      multiple = FALSE),
    htmltools::hr(),
    shiny::tags$h4("Settings"),
    shiny::tags$h5("Select analysis type:"),
    shinyWidgets::pickerInput(
      inputId = ns("selectAnalysisType_pickerInput_gwas"),
      choices = c("additive", "recessive", "dominant"),
      selected = "additive",
      multiple = FALSE),
    shiny::textInput(ns("pheno"), label = "Phenotype Name:"),
    shiny::textInput(ns("description"), label = "Description:"),
    htmltools::hr(),
    shiny::tags$h4("Pre-ran info"),
    shiny::verbatimTextOutput(ns("info_text"), placeholder = TRUE),
    htmltools::hr()
  )
}

#' @title Analysis Settings for GWAS Module Server
#'
#' @description
#' A Shiny module server function that sets up the analysis settings for a GWAS (Genome-Wide Association Study).
#' It manages the user inputs for selecting case and control cohorts, setting a phenotype name, and description,
#' and provides reactive outputs for the analysis settings.
#'
#' @param id A string representing the module's ID.
#' @param r_connectionHandler A reactive object containing database and cohort connection handlers.
#'
#' @return A reactive expression that provides the analysis settings for the GWAS.
#'
#' @importFrom shiny moduleServer observe req updateTextInput updatePickerInput renderText reactive observeEvent
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom shinyFeedback feedbackWarning
#' @importFrom stringr str_detect
#' @importFrom purrr discard
#' @importFrom dplyr filter pull
#' @importFrom ParallelLogger logError logWarn logInfo
#'
#' @export
mod_analysisSettings_GWAS_server <- function(id, r_connectionHandler) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # update selectCaseCohort_pickerInput
    #
    shiny::observe({
      shiny::req(r_connectionHandler$cohortTableHandler)
      shiny::req(r_connectionHandler$hasChangeCounter)

      cohortIdAndNames <- r_connectionHandler$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- list()
      if(nrow(cohortIdAndNames) != 0){
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "(", cohortIdAndNames$cohortName, ")")))
      }

      shinyWidgets::updatePickerInput(
        inputId = "selectCaseCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = character(0)
      )
    })


    #
    # update selectControlCohort_pickerInput
    #
    shiny::observe({
      shiny::req(r_connectionHandler$hasChangeCounter)
      shiny::req(input$selectCaseCohort_pickerInput)

      cohortIdAndNames <- r_connectionHandler$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- list()
      if(nrow(cohortIdAndNames) != 0){
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "(", cohortIdAndNames$cohortName, ")")))
      }

      cohortIdAndNamesList <- cohortIdAndNamesList |>
        purrr::discard(~.x %in% input$selectCaseCohort_pickerInput)

      shinyWidgets::updatePickerInput(
        inputId = "selectControlCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = character(0)
      )
    })


    #
    # update phenotype name and description with default values
    #
    shiny::observe({
      shiny::req(!is.null(input$selectCaseCohort_pickerInput))
      shiny::req(!is.null(input$selectControlCohort_pickerInput))

      cohortTableHandler <- r_connectionHandler$cohortTableHandler

      cohorts  <- cohortTableHandler$getCohortsSummary()
      casesCohortName <- cohorts |> filter(cohortId == input$selectCaseCohort_pickerInput) |> dplyr::pull(cohortName)
      controlsCohortName <- cohorts |> filter(cohortId == input$selectControlCohort_pickerInput) |> dplyr::pull(cohortName)

      defaultPhenotypeName <- paste0(
        casesCohortName |> stringr::str_replace_all("[[:punct:]]|[^[:alnum:]]|[:blank:]", "")  |> toupper(),
        controlsCohortName |> stringr::str_replace_all("[[:punct:]]|[^[:alnum:]]|[:blank:]", "")  |> toupper()
      )
      dbName <- cohortTableHandler$databaseName
      defaultDescription <- paste0("Cases-cohort: ", casesCohortName, "; Controls-cohort: ",
                                   controlsCohortName, " (db: ", dbName, ")")

      shiny::updateTextInput(session, "pheno", value = defaultPhenotypeName )
      shiny::updateTextInput(session, "description", value = defaultDescription )

    })

    #
    # setup warning on input for the phenotype name
    #
    shiny::observeEvent(input$pheno, {
      shinyFeedback::feedbackWarning(
        inputId = "pheno",
        stringr::str_detect(input$pheno, "[^[:alnum:]]|[:lower:]"),
        text = "Name must use only upper case characters or numbers"
      )

    })


    #
    # render info text
    #
    output$info_text <- shiny::renderText({
      shiny::req(input$selectCaseCohort_pickerInput)
      shiny::req(input$selectControlCohort_pickerInput)

      cohortsOverlap <- r_connectionHandler$cohortTableHandler$getCohortsOverlap()
      cohortCounts <- r_connectionHandler$cohortTableHandler$getCohortCounts()

      nSubjectsOverlap <- cohortsOverlap |>
        dplyr::filter(
          stringr::str_detect(cohortIdCombinations, paste0("-", input$selectCaseCohort_pickerInput, "-")) &
            stringr::str_detect(cohortIdCombinations, paste0("-", input$selectControlCohort_pickerInput, "-"))
        ) |>
        dplyr::pull(numberOfSubjects)  |>
        sum()

      nSubjectsCase <- cohortCounts |>
        dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
        dplyr::pull(cohortSubjects)

      nSubjectsControl <- cohortCounts |>
        dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
        dplyr::pull(cohortSubjects)

      nEntriesCase <- cohortCounts |>
        dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
        dplyr::pull(cohortEntries)

      nEntriesControl <- cohortCounts |>
        dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
        dplyr::pull(cohortEntries)

      message <- ""

      # counts
      if( nSubjectsCase > nSubjectsControl ){
        message <- paste0(message, "\u274C There are more subjects in  ase cohort (", nSubjectsCase,") that in control cohort (", nSubjectsControl,"). Are you sure they are correct?\n")
      }

      # overlap
      if(nSubjectsOverlap==0){
        message <- paste0(message, "\u2705 No subjects overlap between case and control cohorts\n")
      }else{
        message <- paste0(message, "\u274C There are ", nSubjectsOverlap, " subjects that overlap  berween case and control cohorts. Consider removing them in Operate Cohorts tab\n")
      }

      # duplicates
      if(nEntriesCase > nSubjectsCase){
        message <- paste0(message, "\u274C There are more entries than subjects in case cohort (", nEntriesCase, " > ", nSubjectsCase, "). Duplicate subject will be ignored.\n")
      }
      if(nEntriesControl > nSubjectsControl){
        message <- paste0(message, "\u274C There are more entries than subjects in control cohort (", nEntriesControl, " > ", nSubjectsControl, "). Duplicate subject will be ignored.\n")
      }

      return(message)

    })


    #
    # return reactive options
    #
    rf_analysisSettings <- shiny::reactive({
      if(is.null(input$selectCaseCohort_pickerInput) |
        is.null(input$selectControlCohort_pickerInput) |
        input$pheno == "" |
        input$description == "" |
        is.null(input$selectAnalysisType_pickerInput_gwas)) {
        return(NULL)
      }

      cohortTableHandler <- r_connectionHandler$cohortTableHandler

      databaseId <- cohortTableHandler$databaseId
      release <- paste0("Regenie", gsub("[A-Za-z]", "", cohortTableHandler$databaseId))

      analysisSettings <- list(
        cohortIdCases = input$selectCaseCohort_pickerInput |> as.integer(),
        cohortIdControls = input$selectControlCohort_pickerInput |> as.integer(),
        phenotype = input$pheno,
        description = input$description,
        analysisType = input$selectAnalysisType_pickerInput_gwas,
        release = release
      )

      return(analysisSettings)

    })

    return(rf_analysisSettings)

  })

}

