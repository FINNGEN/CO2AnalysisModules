
# run module --------------------------------------------------------------
devtools::load_all(".")


analysisIdsToShow <- HadesExtras::getListOfAnalysis() |> dplyr::pull(analysisId)
analysisIdsToShow <- analysisIdsToShow[1:30] 
analysisRegexToShowTibble <- tibble::tribble(
  ~analysisId, ~analysisName, ~analysisRegex,
  999, "Endpoints", "^(?!.*\\[CohortLibrary\\]).*_case$",
  998, "CohortLibrary", ".*\\[CohortLibrary\\]"
)
analysisIdsSelected <- c(analysisIdsToShow[1:3], 999) 


shiny::shinyApp(
  shiny::fluidPage(
    mod_fct_covariateSelector_ui(
      inputId = "test",
      label = "This is a test lable",
      analysisIdsToShow = analysisIdsToShow,
      analysisRegexToShowTibble = analysisRegexToShowTibble,
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
