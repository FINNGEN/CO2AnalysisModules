
test_that("mod_analysisSettings_CohortOverlaps works", {

  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  on.exit({rm(cohortTableHandler);gc()})

  r_connectionHandler <- shiny::reactiveValues(
    cohortTableHandler = cohortTableHandler,
    hasChangeCounter = 0
  )

  # run module
  shiny::testServer(
    mod_analysisSettings_cohortOverlaps_server,
    args = list(
      id = "test",
      r_connectionHandler = r_connectionHandler
    ),
    {
      session$setInputs(selectCohorts_pickerInput = c(1,2), minCellCount_numericInput = 1)

      analysisSettings <- rf_analysisSettings()

      analysisSettings |> assertAnalysisSettings_CohortOverlaps() |> expect_no_error()

    }
  )

})



