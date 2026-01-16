#' @title timeCodeWASs Analysis Settings UI
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
mod_analysisSettings_timeCodeWAS_ui <- function(id) {
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

    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] == '0'",
        ns("selectControlCohort_pickerInput")
      ),
      shiny::tags$div(
        style = "margin-top: 0.5rem;",
        shiny::tags$h5("Auto-matching ratio"),

        shiny::selectizeInput(
          inputId = ns("autoMatch_ratio"),
          label = "Controls per case (ratio)",
          choices = c("10" = 10, "50" = 50, "100" = 100),
          selected = 10,
          options = list(
            create = TRUE,
            persist = FALSE
          )
        ),

        shiny::helpText(
          "Select 10, 50, or 100 controls per case, or enter a custom ratio. ",
          "Larger ratios may significantly increase analysis time. ",
          "The combined case + control size is capped automatically."
        )
      )
    ),


    htmltools::hr(),
    shiny::tags$h4("Settings"),
    #
    mod_fct_covariateSelector_ui(
      inputId = ns("features_pickerInput"),
      label = "Select features to compare between cases and controls:",
      analysisIdsToShow = c(
        101, 102, 141, 204,
        601, 641,
        301, 342, 343, 404, 342,
        701, 702, 703, 741,
        801, 841,
        501, 541,
        910, 911 ),
      analysisIdsSelected = c(141, 641, 342, 701, 702, 841, 541)
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
    shiny::tags$h4("Time windows"),
    mod_fct_formTimeWindows_ui(ns("selectRanges")),
    shiny::tags$br(),
    #
    htmltools::hr(),
    shiny::tags$h4("Pre-run info"),
    shiny::verbatimTextOutput(ns("info_text"), placeholder = TRUE)
  )
}

#' @title timeCodeWASs Analysis Settings Server
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
mod_analysisSettings_timeCodeWAS_server <- function(id, r_connectionHandler) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    rf_ranges <- mod_fct_formTimeWindows_server("selectRanges")

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
        session = session,
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
      cohortIdAndNamesList <- list()
      if(nrow(cohortIdAndNames) != 0){
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "("  , cohortIdAndNames$cohortName, ")")))
      }

      cohortIdAndNamesList <- cohortIdAndNamesList |>
        purrr::discard(~.x %in% input$selectCaseCohort_pickerInput)

      # Add cohort 0 with
      cohortIdAndNamesList <- c(list(`AUTO-MATCH: Creates a control cohort from the patiens not in case cohort that matches case cohort by sex and birth year with selected ratio and start date as in case cohort` = 0), cohortIdAndNamesList)

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selectControlCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = 0
      )
    })

    # Handle automatch ratio selection from user
    autoMatch_ratio <- shiny::reactive({
      shiny::req(input$autoMatch_ratio)
      r <- suppressWarnings(as.integer(input$autoMatch_ratio))
      if (is.na(r) || r < 1) 10L else r
    })

    n_cases <- shiny::reactive({
      shiny::req(input$selectCaseCohort_pickerInput)
      cohortTableHandler <- r_connectionHandler$cohortTableHandler
      cohortTableHandler$getNumberOfSubjects(as.integer(input$selectCaseCohort_pickerInput))
    })

    max_ratio_allowed <- shiny::reactive({
      shiny::req(n_cases())
      cap <- 400000L # total size for codewas analysis
      nc <- as.integer(n_cases())
      if (nc <= 0) return(10L)

      mr <- floor((cap - nc) / nc)
      max(1L, mr)
    })

    shiny::observeEvent(
      list(
        input$selectControlCohort_pickerInput,
        input$selectCaseCohort_pickerInput,
        input$autoMatch_ratio
      ),
      {
        ctrl <- input$selectControlCohort_pickerInput
        if (is.null(ctrl) || length(ctrl) == 0) return()

        if (as.character(ctrl) != "0") return()

        if (is.null(input$autoMatch_ratio) || length(input$autoMatch_ratio) == 0) return()

        r <- suppressWarnings(as.integer(input$autoMatch_ratio))
        if (is.na(r) || r < 1) r <- 10L

        mr <- max_ratio_allowed()

        if (r > mr) {
          shiny::showNotification(
            paste0(
              "Auto-matching ratio capped to ", mr,
              " to keep total analyzed sample size ≤ 400,000."
            ),
            type = "warning",
            duration = 6
          )

          shiny::updateSelectizeInput(
            session,
            "autoMatch_ratio",
            selected = as.character(mr)
          )
        }
      },
      ignoreInit = TRUE
    )


    #
    # activate settings if cohors have been selected
    #
    shiny::observe({
      ctrl <- input$selectControlCohort_pickerInput
      condition <- !is.null(ctrl) && length(ctrl) == 1 && as.character(ctrl) != "NA"

      #shinyjs::toggleState("selectCovariates", condition = condition )
      shinyjs::toggleState("minCellCount_numericInput", condition = condition )
    })


    #
    # Create advice message
    #
    output$info_text <- shiny::renderText({
       if (!shiny::isTruthy(r_connectionHandler$hasChangeCounter) ||
      !shiny::isTruthy(input$selectCaseCohort_pickerInput) ||
      !shiny::isTruthy(input$selectControlCohort_pickerInput)) {
        return("")
      }

      cohortTableHandler <- r_connectionHandler$cohortTableHandler


      nSubjectsOverlap <- cohortTableHandler$getNumberOfOverlappingSubjects(selected_cohortId1=input$selectCaseCohort_pickerInput,selected_cohortId2=input$selectControlCohort_pickerInput)
      nSubjectsCase <- cohortTableHandler$getNumberOfSubjects(input$selectCaseCohort_pickerInput)
      nSubjectsControl <- cohortTableHandler$getNumberOfSubjects(input$selectControlCohort_pickerInput)
      nEntryCase <- cohortTableHandler$getNumberOfCohortEntries(input$selectCaseCohort_pickerInput)
      nEntryControl <- cohortTableHandler$getNumberOfCohortEntries(input$selectControlCohort_pickerInput)

      message <- ""

      if (as.character(input$selectControlCohort_pickerInput) == "0") {
        ratio_txt <- if(shiny::isTruthy(input$autoMatch_ratio)) input$autoMatch_ratio else "10"
        message <- paste0(
          message,
          "\u2139\uFE0F Analysis will create an auto-matched control cohort (sex + birth year) with ratio 1:",
          ratio_txt, " and start date as in case cohort. \n"
        )
        return(message)
      }

      # counts
      if( nEntryCase > nEntryControl ){
        message <- paste0(message, "There are more entries in case cohort (", nEntryCase,") than in control cohort (", nEntryControl,"). Are you sure they are correct?\n")
      }

      # overlap
      if(nSubjectsOverlap==0){
        message <- paste0(message, "\u2705 No subjects overlap between case and control cohorts\n")
      }else{
        if(nSubjectsOverlap > nSubjectsCase * .20){
          message <- paste0(message, "\u274C There are many subjects, ",nSubjectsOverlap, ", that overlap  berween case and control cohorts. Consider removing them in Operate Cohorts tab\n")
        }else{
          message <- paste0(message, "\u26A0\uFE0F There are few subjects, ",nSubjectsOverlap, ", that overlap between case and control cohorts. \n")
        }
      }

      # sex
      fisher_results = cohortTableHandler$getSexFisherTest(selected_cohortId1=input$selectCaseCohort_pickerInput,
                                                           selected_cohortId2=input$selectControlCohort_pickerInput,
                                                           testFor="allEvents")

      if(fisher_results$p.value < 0.05){
        message <- paste0(message, "\u26A0\uFE0F There is a significant difference in sex distribution between case and control cohorts. (Fisher's test p = ", scales::scientific(fisher_results$p.value)," ) \n")
        message <- paste0(message, "Consider controling for sex  creating a new control cohort that match case cohort by sex in the Match Cohorts tab\n")
      }


      # year of birth
      yearOfBirthComparison_results = cohortTableHandler$getYearOfBirthTests(selected_cohortId1=input$selectCaseCohort_pickerInput,
                                                                             selected_cohortId2=input$selectControlCohort_pickerInput,
                                                                             testFor="allEvents")

      ttestResult <- yearOfBirthComparison_results[["ttestResult"]]
      ks_result <- yearOfBirthComparison_results[["ksResult"]]
      cohen_d  <- yearOfBirthComparison_results[["cohendResult"]]["cohend"]
      meanCases <- yearOfBirthComparison_results[["cohendResult"]]["meanInCases"]
      meanControls <- yearOfBirthComparison_results[["cohendResult"]]["meanInControls"]
      pooled_sd <- yearOfBirthComparison_results[["cohendResult"]]["pooledsd"]

      p_ttest <- ttestResult$p.value
      p_ks <- ks_result$p.value
      d <- abs(cohen_d)

      sig_t <- p_ttest < 0.05
      sig_ks <- p_ks < 0.05
      small_d <- d < 0.2

      if (sig_ks && !sig_t) {
        message <- paste0(message,
                          "\u26A0\uFE0F There is a significant difference in the shapes of year of birth distributions between the case and control cohorts (KS test), but the mean year of births are similar (t-test).\n",
                          "- t-test p = ", scales::scientific(p_ttest), "\n",
                          "- KS test p = ", scales::scientific(p_ks), "\n",
                          "- Cohen's d = ", round(d, 3), " (mean year of birth: Cases=",round(meanCases),", Controls=",round(meanControls),". Cohen's d, the effect size of the difference in means is negligible if less than 0.2) \n"
        )
        message <- paste0(message, "\n=> Consider creating a matched control cohort by year of birth (in the Match Cohorts tab) if the Cohen's d effect size is greater than 0.2. \n")

      } else if (sig_t && !sig_ks) {
        message <- paste0(message,
                          "\u26A0\uFE0F There is a significant difference in the mean year of birth between case and control cohorts (t-test), but year of birth distributions are similar in shape (KS test).\n",
                          "- t-test p = ", scales::scientific(p_ttest), "\n",
                          "- KS test p = ", scales::scientific(p_ks), "\n",
                          "- Cohen's d = ", round(d, 3), " (mean year of birth: Cases=",round(meanCases),", Controls=",round(meanControls),". Cohen's d, the effect size of the difference in means is negligible if less than 0.2) \n"
        )
        if (small_d) {
          message <- paste0(message, "\u2139\uFE0F Effect size is small - practical difference may be negligible.\n")
        } else {
          message <- paste0(message, "\n=> Consider controlling for year of birth by creating a new control cohort matching the case cohort by year of birth in the Match Cohorts tab.\n")
        }
      } else if (sig_t && sig_ks) {
        message <- paste0(message,
                          "\u26A0\uFE0F There is significant difference both in the mean year of birth between case and control cohorts (t-test) - and the shapes of year of birth distributions (KS test).\n",
                          "- t-test p = ", scales::scientific(p_ttest), "\n",
                          "- KS test p = ", scales::scientific(p_ks), "\n",
                          "- Cohen's d = ", round(d, 3)," (mean year of birth: Cases=",round(meanCases),", Controls=",round(meanControls),". Cohen's d, the effect size of the difference in means is negligible if less than 0.2)  \n"
        )
        if (small_d) {
          message <- paste0(message, "\u2139\uFE0F Effect size is small - practical difference may be negligible but the birth year distributions are significantly different .\n")
        }
        message <- paste0(message, "\n=> Consider controlling for year of birth by creating a new control cohort matching the case cohort by year of birth in the Match Cohorts tab.\n")
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
        is.null(rf_ranges()) |
        is.null(input$minCellCount_numericInput)
      ){
        return(NULL)
      }

      is_automatched <- as.character(input$selectControlCohort_pickerInput) == "0"
      analysisSettings <- list(
        cohortIdCases = input$selectCaseCohort_pickerInput |> as.integer(),
        cohortIdControls = input$selectControlCohort_pickerInput |> as.integer(),
        autoMatchRatio = if (is_automatched) autoMatch_ratio() else NULL,
        analysisIds = input$features_pickerInput |> as.integer(),
        temporalStartDays = rf_ranges()$temporalStartDays,
        temporalEndDays =   rf_ranges()$temporalEndDays
      )

      return(analysisSettings)
    })

    return(rf_analysisSettings)
  })
}





















