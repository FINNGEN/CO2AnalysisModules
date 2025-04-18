# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")


# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_analysisSettings_GWAS_ui("test")
  ),
  function(input, output, session) {
    r_connectionHandler <- shiny::reactiveValues(
      cohortTableHandler = cohortTableHandler,
      hasChangeCounter = 0
    )


    rf_analysisSettings <- mod_analysisSettings_GWAS_server("test", r_connectionHandler)

    shiny::observe({
      analysisSettings <- rf_analysisSettings()
      print(analysisSettings)
      if (!is.null(analysisSettings)) {
        # analysisSettings |>
        #   assertAnalysisSettings_GWAS() |>
        #   expect_no_error()
      }
    })
  },
  options = list(launch.browser = TRUE)
)

app
