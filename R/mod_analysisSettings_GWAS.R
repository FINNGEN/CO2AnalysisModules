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
        `actions-box` = TRUE
      ),
      multiple = FALSE
    ),
    shiny::tags$h5("Select control cohort:"),
    shinyWidgets::pickerInput(
      inputId = ns("selectControlCohort_pickerInput"),
      label = NULL,
      choices = NULL,
      selected = NULL,
      options = list(
        `actions-box` = TRUE
      ),
      multiple = FALSE
    ),
    htmltools::hr(),
    shiny::tags$h4("Settings"),
    shiny::tags$h5("Select analysis type:"),
    shinyWidgets::pickerInput(
      inputId = ns("selectAnalysisType_pickerInput"),
      choices = c("additive", "recessive", "dominant"),
      selected = "additive",
      multiple = FALSE
    ),
    shiny::textInput(ns("phenotypeName_textInput"), label = "Phenotype Name:"),
    shiny::textInput(ns("description_textInput"), label = "Description:"),
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
#' @importFrom shiny moduleServer observe req updateTextInput renderText reactive observeEvent
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

    r <- shiny::reactiveValues(
      connectionSandboxAPI = NULL
    )

    #
    # update selectCaseCohort_pickerInput
    #
    shiny::observe({
      shiny::req(r_connectionHandler$cohortTableHandler)
      shiny::req(r_connectionHandler$hasChangeCounter)

      cohortIdAndNames <- r_connectionHandler$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- list()
      if (nrow(cohortIdAndNames) != 0) {
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
      if (nrow(cohortIdAndNames) != 0) {
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "(", cohortIdAndNames$cohortName, ")")))
      }

      cohortIdAndNamesList <- cohortIdAndNamesList |>
        purrr::discard(~ .x %in% input$selectCaseCohort_pickerInput)

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

      cohorts <- cohortTableHandler$getCohortsSummary()
      casesCohortName <- cohorts |>
        filter(cohortId == input$selectCaseCohort_pickerInput) |>
        dplyr::pull(cohortName)
      controlsCohortName <- cohorts |>
        filter(cohortId == input$selectControlCohort_pickerInput) |>
        dplyr::pull(cohortName)

      defaultPhenotypeName <- paste0(
        casesCohortName |> stringr::str_replace_all("[[:punct:]]|[^[:alnum:]]|[:blank:]", "") |> toupper(),
        controlsCohortName |> stringr::str_replace_all("[[:punct:]]|[^[:alnum:]]|[:blank:]", "") |> toupper(),
        input$selectAnalysisType_pickerInput |> toupper()
      )
      dbName <- cohortTableHandler$databaseName
      defaultDescription <- paste0(
        "Cases-cohort: ", casesCohortName, "; Controls-cohort: ",
        controlsCohortName, " (db: ", dbName, ")",
        "; Analysis type: ", input$selectAnalysisType_pickerInput
      )

      shiny::updateTextInput(session, "phenotypeName_textInput", value = defaultPhenotypeName)
      shiny::updateTextInput(session, "description_textInput", value = defaultDescription)
    })

    #
    # setup warning on input for the phenotype name
    #
    shiny::observeEvent(input$phenotypeName_textInput, {
      shinyFeedback::feedbackWarning(
        inputId = "phenotypeName_textInput",
        stringr::str_detect(input$phenotypeName_textInput, "[^[:alnum:]]|[:lower:]"),
        text = "Name must use only upper case characters or numbers"
      )
    })


    #
    # render info text
    #
    output$info_text <- shiny::renderText({
      if ( !shiny::isTruthy(input$selectCaseCohort_pickerInput) ||
      !shiny::isTruthy(input$selectControlCohort_pickerInput)) {
        return("")
      }

      nSubjectsOverlap <- r_connectionHandler$cohortTableHandler$getNumberOfOverlappingSubjects(selected_cohortId1=input$selectCaseCohort_pickerInput,selected_cohortId2=input$selectControlCohort_pickerInput)
      nSubjectsCase <- r_connectionHandler$cohortTableHandler$getNumberOfSubjects(input$selectCaseCohort_pickerInput)
      nSubjectsControl <- r_connectionHandler$cohortTableHandler$getNumberOfSubjects(input$selectControlCohort_pickerInput)
      nEntriesCase <- r_connectionHandler$cohortTableHandler$getNumberOfCohortEntries(input$selectCaseCohort_pickerInput)
      nEntriesControl <- r_connectionHandler$cohortTableHandler$getNumberOfCohortEntries(input$selectControlCohort_pickerInput)

      message <- ""

      # check token
      connectionSandboxAPI <- NULL
      if (!file.exists(".sandbox_token")) {
        message <- paste0(message, "\u274C Token not found.\n")
      } else {
        # try to chmod .sandbox_token
        Sys.chmod(".sandbox_token", mode = "666")
        token <- readLines(".sandbox_token")
        base_url <- "https://internal-api.app.finngen.fi/internal-api/"
        connectionSandboxAPI <- FinnGenUtilsR::createSandboxAPIConnection(base_url, token)
        if (connectionSandboxAPI$conn_status_tibble$logTibble$type == "ERROR") {
          message <- paste0(message, "\u274C Error connecting to the sandbox API. Error message: ", connectionSandboxAPI$conn_status_tibble$logTibble$message, "\n")
          connectionSandboxAPI <- NULL
        } else {
          message <- paste0(message, "\u2705 Token found and connection to the sandbox API successful\n")
          message <- paste0(message, "Results will be sent to: ", connectionSandboxAPI$notification_email, "\n")
        }
      }



      # counts
      if (nSubjectsCase > nSubjectsControl) {
        message <- paste0(message, "\u274C There are more subjects in  ase cohort (", nSubjectsCase, ") that in control cohort (", nSubjectsControl, "). Are you sure they are correct?\n")
      }

      # overlap
      if (nSubjectsOverlap == 0) {
        message <- paste0(message, "\u2705 No subjects overlap between case and control cohorts\n")
      } else {
        message <- paste0(message, "\u274C There are ", nSubjectsOverlap, " subjects that overlap  berween case and control cohorts. Consider removing them in Operate Cohorts tab\n")
      }

      # duplicates
      if (nEntriesCase > nSubjectsCase) {
        message <- paste0(message, "\u274C There are more entries than subjects in case cohort (", nEntriesCase, " > ", nSubjectsCase, "). Duplicate subject will be ignored.\n")
      }
      if (nEntriesControl > nSubjectsControl) {
        message <- paste0(message, "\u274C There are more entries than subjects in control cohort (", nEntriesControl, " > ", nSubjectsControl, "). Duplicate subject will be ignored.\n")
      }

      # sex
      fisher_results = r_connectionHandler$cohortTableHandler$getSexFisherTest(selected_cohortId1=input$selectCaseCohort_pickerInput,
                                                                               selected_cohortId2=input$selectControlCohort_pickerInput)


      # year of birth
      yearOfBirthComparison_results = r_connectionHandler$cohortTableHandler$getYearOfBirthTests(selected_cohortId1=input$selectCaseCohort_pickerInput,
                                                                                                 selected_cohortId2=input$selectControlCohort_pickerInput)

      ttestResult <- yearOfBirthComparison_results[["ttestResult"]]


      if (fisher_results$p.value > 0.05 & ttestResult$p.value > 0.05) {
        message <- paste0(message, "\u26A0\uFE0F Case and control cohorts seem to have the same sex and year of birth distribution. \n")
        message <- paste0(message, "It is not recommended to run GWAS with explicitly matched cohorts. GWAS analysis accounts for sex and year of birth in the model. Explicit matching may introduce bias.\n")
      }

      r$connectionSandboxAPI <- connectionSandboxAPI
      return(message)
    })


    #
    # return reactive options
    #
    rf_analysisSettings <- shiny::reactive({
      if (is.null(input$selectCaseCohort_pickerInput) |
        is.null(input$selectControlCohort_pickerInput) |
        input$phenotypeName_textInput == "" |
        input$description_textInput == "" |
        is.null(input$selectAnalysisType_pickerInput) |
        is.null(r$connectionSandboxAPI)) {
        return(NULL)
      }

      cohortTableHandler <- r_connectionHandler$cohortTableHandler
      connectionSandboxAPI <- r$connectionSandboxAPI

      databaseId <- cohortTableHandler$databaseId
      release <- paste0("Regenie", gsub("[A-Za-z]", "", cohortTableHandler$databaseId))

      analysisSettings <- list(
        cohortIdCases = input$selectCaseCohort_pickerInput |> as.integer(),
        cohortIdControls = input$selectControlCohort_pickerInput |> as.integer(),
        phenotype = input$phenotypeName_textInput,
        description = input$description_textInput,
        analysisType = input$selectAnalysisType_pickerInput,
        release = release,
        connectionSandboxAPI = connectionSandboxAPI
      )

      return(analysisSettings)
    })

    return(rf_analysisSettings)
  })
}
