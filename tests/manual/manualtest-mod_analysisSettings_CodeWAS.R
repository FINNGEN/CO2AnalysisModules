# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohortsMatched")

# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_analysisSettings_codeWAS_ui("test")
  ),
  function(input, output, session) {
    r_connectionHandler <- shiny::reactiveValues(
      cohortTableHandler = cohortTableHandler,
      hasChangeCounter = 0
    )

    rf_analysisSettings <- mod_analysisSettings_codeWAS_server("test", r_connectionHandler)

    shiny::observe({
      analysisSettings <- rf_analysisSettings()
      print(analysisSettings)
      if (!is.null(analysisSettings)) {
        analysisSettings |>
          assertAnalysisSettings_CodeWAS() |>
          expect_no_error()
      }
    })
  },
  options = list(launch.browser = TRUE)
)

app
