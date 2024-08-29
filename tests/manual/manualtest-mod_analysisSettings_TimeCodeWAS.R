
# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohortsMatched")

r_connectionHandler <- shiny::reactiveValues(
  cohortTableHandler = cohortTableHandler,
  hasChangeCounter = 0
)

# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_analysisSettings_timeCodeWAS_ui("test")
  ),
  function(input,output,session){
    rf_analysisSettings <- mod_analysisSettings_timeCodeWAS_server("test", r_connectionHandler)

    shiny::observe({
      print(rf_analysisSettings())
    })
  },
  options = list(launch.browser=TRUE)
)

app
