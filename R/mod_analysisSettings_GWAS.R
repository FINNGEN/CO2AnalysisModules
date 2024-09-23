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


mod_analysisSettings_GWAS_server <- function(id, r_connectionHandler, r_workbench) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # update selectCaseCohort_pickerInput
    #
    shiny::observe({
      shiny::req(r_connectionHandler$cohortTableHandler)
      shiny::req(r_connectionHandler$hasChangeCounter)

      cohortIdAndNames <- r_connectionHandler$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, cohortIdAndNames$cohortName))

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
      cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, cohortIdAndNames$cohortName))

      cohortIdAndNamesList <- cohortIdAndNamesList |>
        purrr::discard(~.x %in% input$selectCaseCohort_pickerInput)

      shinyWidgets::updatePickerInput(
        inputId = "selectControlCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = character(0)
      )
    })


    #
    # update matchToCohortId_pickerInput with cohort names not in selectCaseCohort_pickerInput
    #
    shiny::observe({
      shiny::req(input$selectDatabases_pickerInput)
      shiny::req(input$selectCaseCohort_pickerInput)

      if(input$selectCaseCohort_pickerInput != "NA"){
        cohortIdAndNames <- r_connectionHandlers$cohortTableHandler$getCohortIdAndNames() |>
          dplyr::filter(!(cohortId %in% input$selectCaseCohort_pickerInput))
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "➖"  , cohortIdAndNames$cohortName)))
      }else{
        cohortIdAndNamesList <- list()
      }

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

      cohorts  <- cohortTableHandler$getCohortsSummary()
      casesCohort <- cohorts[cohorts$cohortId == input$selectCaseCohort_pickerInput, ] |> dplyr::pull(cohortName)
      controlsCohort <- cohorts[cohorts$cohortId == input$selectControlCohort_pickerInput, ] |> dplyr::pull(cohortName)

      defaultPhenotypeName <- paste0(.format_str(casesCohort),.format_str(controlsCohort))
      dbName <- r_connectionHandler$cohortTableHandler$databaseName
      defaultDescription <- paste0("Cases-cohort: ", casesCohort, "; Controls-cohort: ",
                                   controlsCohort, " (db: ", dbName, ")")

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

      message <- ""

      # counts
      if( nSubjectsCase > nSubjectsControl ){
        message <- paste0(message, "❌ There are more subjects in  ase cohort (", nSubjectsCase,") that in control cohort (", nSubjectsControl,"). Are you sure they are correct?\n")
      }

      # overlap
      if(nSubjectsOverlap==0){
        message <- paste0(message, "✅ No subjects overlap between case and control cohorts\n")
      }else{
        if(nSubjectsOverlap > nSubjectsCase * .20){
          message <- paste0(message, "❌ There are many subjects, ",nSubjectsOverlap, ", that overlap  berween case and control cohorts. Consider removing them in Operate Cohorts tab\n")
        }else{
          message <- paste0(message, "⚠️ There are few subjects, ",nSubjectsOverlap, ", that overlap between case and control cohorts. \n")
        }
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

      databaseId <- cohortTableHandler$databaseId
      release <- paste0("Regenie", gsub("[A-Za-z]", "", cohortTableHandler$databaseId))

      analysisSettings <- list(
        casesCohort = input$selectCaseCohort_pickerInput |> as.integer(),
        controlsCohort = input$selectControlCohort_pickerInput |> as.integer(),
        phenotype = input$pheno,
        description = input$description,
        analysisType = input$selectAnalysisType_pickerInput_gwas,
        release = release,
        connectionSandboxAPI = r_connectionHandler$connection_sandboxAPI
      )

      return(analysisSettings)

    })

    return(rf_analysisSettings)

  })

}


.format_str <- function(x){
  toupper(stringr::str_replace_all(x, "[[:punct:]]|[^[:alnum:]]|[:blank:]", ""))
}
