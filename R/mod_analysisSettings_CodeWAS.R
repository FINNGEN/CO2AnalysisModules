#' @title CodeWASs Analysis Settings UI
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
mod_analysisSettings_codeWAS_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Cohorts"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCaseCohort_pickerInput"),
      label = "Select case cohort:",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    shinyWidgets::pickerInput(
      inputId = ns("selectControlCohort_pickerInput"),
      label = "Select control cohort:",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    htmltools::hr(),
    shiny::tags$h4("Settings"),
    #
    mod_fct_covariateSelector_ui(
      inputId = ns("features_pickerInput"),
      label = "Select features to compare between cases and controls:",
      analysisIdsToShow = c(101, 102, 141, 204, 1, 2, 3, 6, 8, 9, 10, 41, 601, 641, 301, 341, 404, 906, 701, 702, 703, 741, 908, 801, 841, 909,501, 541, 907, 910   ),
      analysisIdsSelected = c(141, 1, 2, 8, 10, 41, 641, 341, 404, 741, 841, 541 )
    ),
    shinyWidgets::radioGroupButtons(
      inputId = ns("statistics_type_option"),
      label = "Select comparison statistics:",
      choices = list(
        `Fisher's exact test and Welch's t-test (fast, no control for confounding)` = "aggregated",
        `(BETA)Logistic and linear regresions (slow, control for confounding)` = "full"
      ),
      direction = "vertical",
      individual = TRUE,
      checkIcon = list(
        yes = shiny::tags$i(class = "fa fa-circle",
                            style = "color: steelblue"),
        no = shiny::tags$i(class = "fa fa-circle-o",
                           style = "color: steelblue"))
    ),
    # conditional panel
    shiny::conditionalPanel(
      condition = "input.statistics_type_option == 'full'",
      ns = ns,
      shiny::tags$h5("Confounding variables: "),
      shiny::checkboxInput(
        inputId = ns("controlSex_checkboxInput"),
        label = "Sex",
        value = TRUE
      ),
      shiny::checkboxInput(
        inputId = ns("controlYearOfBirth_checkboxInput"),
        label = "Year of birth",
        value = TRUE
      ),
    ),
    shiny::tags$h5("Minimum cell count:"),
    shiny::numericInput(
      inputId = ns("minCellCount_numericInput"),
      label = NULL,
      value = 1,
      min = 1,
      max = 1000
    ),
    htmltools::hr(),
    shiny::tags$h4("Pre-ran info"),
    shiny::verbatimTextOutput(ns("info_text"), placeholder = TRUE)
  )
}

#' @title CodeWASs Analysis Settings Server
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
mod_analysisSettings_codeWAS_server <- function(id, r_connectionHandler) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # TEMP
    cores <- 1#shiny::getShinyOption("cores")
    chunksSizeNOutcomes <- 5000#shiny::getShinyOption("chunksSizeNOutcomes")
    # END TEMP

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
      cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, cohortIdAndNames$cohortName))

      shinyWidgets::updatePickerInput(
        inputId = "selectCaseCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = character(0)
      )
    })

    #
    # update matchToCohortId_pickerInput with cohort names not in selectCaseCohort_pickerInput
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
    # activate settings if cohors have been selected
    #
    shiny::observe({
      condition <- !is.null(input$selectControlCohort_pickerInput) & input$selectControlCohort_pickerInput!="NA"
      #shinyjs::toggleState("selectCovariates", condition = condition )
      shinyjs::toggleState("features_pickerInput", condition = condition )
      shinyjs::toggleState("statistics_type_option", condition = condition )
      shinyjs::toggleState("minCellCount_numericInput", condition = condition )
    })


    #
    # Create advice message
    #
    output$info_text <- shiny::renderText({
      shiny::req(r_connectionHandler$hasChangeCounter)
      shiny::req(input$selectCaseCohort_pickerInput)
      shiny::req(input$selectControlCohort_pickerInput)

      cohortTableHandler <- r_connectionHandler$cohortTableHandler

      cohortsOverlap <- cohortTableHandler$getCohortsOverlap()
      cohortCounts <-  cohortTableHandler$getCohortCounts()
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

      cohortsSumary  <- cohortTableHandler$getCohortsSummary()

      # cores
      message <- paste0("🔢 Analysis will use : ", cores, " cores\n")

      # counts
      if( nSubjectsCase > nSubjectsControl ){
        message <- paste0(message, "❌ There are more subjects in  case cohort (", nSubjectsCase,") that in control cohort (", nSubjectsControl,"). Are you sure they are correct?\n")
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

      # sex
      if(!(input$statistics_type_option == "full" & input$controlSex_checkboxInput)){
        sexCase <- cohortsSumary |>
          dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
          dplyr::pull(sexCounts)
        sexControl <- cohortsSumary |>
          dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
          dplyr::pull(sexCounts)
        nMaleCases <- sexCase[[1]]  |> dplyr::filter(sex == "MALE")  |> dplyr::pull(n)
        nMaleCases <- ifelse(length(nMaleCases)==0, 0, nMaleCases)
        nFemaleCases <- sexCase[[1]]  |> dplyr::filter(sex == "FEMALE")  |> dplyr::pull(n)
        nFemaleCases <- ifelse(length(nFemaleCases)==0, 0, nFemaleCases)
        nMaleControls <- sexControl[[1]]  |> dplyr::filter(sex == "MALE") |> dplyr::pull(n)
        nMaleControls <- ifelse(length(nMaleControls)==0, 0, nMaleControls)
        nFemaleControls <- sexControl[[1]]  |> dplyr::filter(sex == "FEMALE") |> dplyr::pull(n)
        nFemaleControls <- ifelse(length(nFemaleControls)==0, 0, nFemaleControls)

        data <-matrix(c(nMaleCases,nFemaleCases,nMaleControls,nFemaleControls),ncol=2)
        fisher_results <- stats::fisher.test(data)

        if(fisher_results$p.value < 0.05){
          message <- paste0(message, "⚠️ There is a significant difference in sex distribution between case and control cohorts. (Fisher's test p = ", scales::scientific(fisher_results$p.value)," ) \n")
          message <- paste0(message, "Consider controling for sex using regresion statistics or creating a new control cohort that match case cohort by sex in the Match Cohorts tab\n")
        }
      }

      # year of birth
      if(!(input$statistics_type_option == "full" & input$controlYearOfBirth_checkboxInput)){
        yearOfBirthCase <- cohortsSumary |>
          dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
          dplyr::pull(histogramBirthYear)
        yearOfBirthControl <- cohortsSumary |>
          dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
          dplyr::pull(histogramBirthYear)

        ttestResult <- t.test(yearOfBirthCase[[1]]  |> tidyr::uncount(n), yearOfBirthControl[[1]]  |> tidyr::uncount(n))

        if(ttestResult$p.value < 0.05){
          message <- paste0(message, "⚠️ There is a significant difference in year of birth distribution between case and control cohorts. (t-test p = ", scales::scientific(ttestResult$p.value)," ) \n")
          message <- paste0(message, "Consider controling for year of birth using regresion statistics or creating a new control cohort that match case cohort by year of birth in the Match Cohorts tab\n")
        }
      }
      return(message)
    })




    #
    # return reactive options
    #
    rf_analysisSettings <- shiny::reactive({
      if(
        is.null(input$selectCaseCohort_pickerInput) |
        is.null(input$selectControlCohort_pickerInput) |
        is.null(input$features_pickerInput) |
        is.null(input$statistics_type_option) |
        is.null(input$minCellCount_numericInput)
      ){
        return(NULL)
      }

      # if covariates selected, also add the necessary analysis
      covariatesIds <- NULL
      analysisIds  <-  input$features_pickerInput |> as.numeric()
      if(input$statistics_type_option == "full" & input$controlSex_checkboxInput){
        covariatesIds <- c(covariatesIds, 8507001)
        analysisIds <- union(analysisIds, 1)
      }
      if(input$statistics_type_option == "full" & input$controlYearOfBirth_checkboxInput){
        covariatesIds <- c(covariatesIds, 1041)
        analysisIds <- union(analysisIds, 41)
      }

      analysisSettings <- list(
        cohortIdCases = input$selectCaseCohort_pickerInput |> as.integer(),
        cohortIdControls = input$selectControlCohort_pickerInput |> as.integer(),
        analysisIds = analysisIds,
        covariatesIds = covariatesIds,
        minCellCount = input$minCellCount_numericInput,
        chunksSizeNOutcomes = chunksSizeNOutcomes,
        cores = cores
      )

      return(analysisSettings)

    })

    return(rf_analysisSettings)

  })
}





















