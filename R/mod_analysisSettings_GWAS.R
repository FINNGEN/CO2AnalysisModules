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
    # update matchToCohortId_pickerInput with cohort names not in selectCaseCohort_pickerInput
    #
    shiny::observe({
      shiny::req(input$selectDatabases_pickerInput)
      shiny::req(input$selectCaseCohort_pickerInput)

      cohortIdAndNames <- r_connectionHandler$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- list()
      if(nrow(cohortIdAndNames) != 0){
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "("  , cohortIdAndNames$cohortName, ")")))
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
      casesCohort <- cohorts[cohorts$cohortId == input$selectCaseCohort_pickerInput, ] |> dplyr::pull(cohortName)
      controlsCohort <- cohorts[cohorts$cohortId == input$selectControlCohort_pickerInput, ] |> dplyr::pull(cohortName)

      defaultPhenotypeName <- paste0(.format_str(casesCohort),.format_str(controlsCohort))
      dbName <- cohortTableHandler$databaseName
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
        message <- paste0(message, "\u274C There are more subjects in  ase cohort (", nSubjectsCase,") that in control cohort (", nSubjectsControl,"). Are you sure they are correct?\n")
      }

      # overlap
      if(nSubjectsOverlap==0){
        message <- paste0(message, "\u2705 No subjects overlap between case and control cohorts\n")
      }else{
        if(nSubjectsOverlap > nSubjectsCase * .20){
          message <- paste0(message, "\u274C Thereare many subjects, ",nSubjectsOverlap, ", that overlap  berween case and control cohorts. Consider removing them in Operate Cohorts tab\n")
        }else{
          message <- paste0(message, "\u26A0\uFE0F There are few subjects, ",nSubjectsOverlap, ", that overlap between case and control cohorts. \n")
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

      cohortTableHandler <- r_connectionHandler$cohortTableHandler

      databaseId <- cohortTableHandler$databaseId
      release <- paste0("Regenie", gsub("[A-Za-z]", "", cohortTableHandler$databaseId))

      # get connection sandbox API configured for running GWAS
      connectionSandboxAPI <- NULL
      tryCatch({
        connectionSandboxAPI <- .configGWAS()
      }, error=function(e) {
        ParallelLogger::logError("[configGWAS]: ", e$message)
      }, warning=function(w) {
        ParallelLogger::logWarn("[configGWAS]: ", w$message)
      })

      ParallelLogger::logInfo(
        "[configGWAS] Fetched user email:", connectionSandboxAPI$notification_email
      )

      analysisSettings <- list(
        casesCohort = input$selectCaseCohort_pickerInput |> as.integer(),
        controlsCohort = input$selectControlCohort_pickerInput |> as.integer(),
        phenotype = input$pheno,
        description = input$description,
        analysisType = input$selectAnalysisType_pickerInput_gwas,
        release = release,
        connectionSandboxAPI = connectionSandboxAPI
      )

      return(analysisSettings)

    })

    return(rf_analysisSettings)

  })

}


.format_str <- function(x){
  toupper(stringr::str_replace_all(x, "[[:punct:]]|[^[:alnum:]]|[:blank:]", ""))
}


.configGWAS <- function() {

  # if different version of openssl package is used in docker and URL host
  # there will be an error. To avoid the error set up the following configs
  httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

  base_url <- "https://internal-api.app.finngen.fi/internal-api/"

  # get token from env variable
  token <- Sys.getenv('SANDBOX_TOKEN')

  # refresh the token
  authorization <- paste("Bearer", token)
  headers <- httr::add_headers(c('Authorization' = authorization))
  url <- paste0(base_url, "v2/user/refresh-token")

  # fetch refreshed token
  tryCatch({
    res <- httr::GET(url, config = headers)
  }, error = function(e){
    ParallelLogger::logError("[configGWAS] error when refreshing the token", e$message)
  })

  # update token in the environment variable
  if(res$status_code == 200){
    token <- jsonlite::fromJSON(rawToChar(res$content))$token
    Sys.setenv(SANDBOX_TOKEN = token)
  }

  connectionSandboxAPI <- FinnGenUtilsR::createSandboxAPIConnection(base_url, token)

  return(connectionSandboxAPI)
}

