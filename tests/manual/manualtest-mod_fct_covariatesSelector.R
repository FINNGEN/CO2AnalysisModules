
# run module --------------------------------------------------------------
devtools::load_all(".")


analysisIdsToShow <- HadesExtras::getListOfAnalysis() |> dplyr::pull(analysisId)
analysisIdsToShow <- analysisIdsToShow[1:30] 
analysisIdsSelected <- c(analysisIdsToShow[1:3] |> as.character(), "^(?!.*\\[CohortLibrary\\]).*$")
analysisRegexToShow <- tibble::tribble(
  ~analysisName, ~analysisRegex,
  "Endpoints", "^(?!.*\\[CohortLibrary\\]).*$",
  "Cohorts in CohortLibrary", ".*\\[CohortLibrary\\]"
)


shiny::shinyApp(
  shiny::fluidPage(
    mod_fct_covariateSelector_ui(
      inputId = "test",
      label = "This is a test lable",
      analysisIdsToShow = analysisIdsToShow,
      analysisRegexToShow = analysisRegexToShow,
      analysisIdsSelected = analysisIdsSelected
      )
  ),
  function(input,output,session){


    observe({
      print(input$test)
    })

  },
  options = list(launch.browser=TRUE)
)
