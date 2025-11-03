
test_that("mod_analysisSettings_CohortDemographics works", {
  skip_if(testingDatabase != "Eunomia-GiBleed", "Skip test, it is only for Eunomia-GiBleed")
  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({rm(cohortTableHandler);gc()})

  r_connectionHandler <- shiny::reactiveValues(
    cohortTableHandler = cohortTableHandler,
    hasChangeCounter = 0
  )

  # run module
  shiny::testServer(
    mod_analysisSettings_cohortDemographics_server,
    args = list(
      id = "test",
      r_connectionHandler = r_connectionHandler
    ),
    {
      session$setInputs(
        selectCohorts_pickerInput = c(1,2),
        groupBy_pickerInput =  c("calendarYear", "ageGroup", "gender"),
        referenceYears_pickerInput = c("cohort_start_date", "cohort_end_date", "birth_datetime"),
        minCellCount_numericInput = 1
      )

      analysisSettings <- rf_analysisSettings()

      analysisSettings |> assertAnalysisSettings_CohortDemographics() |> expect_no_error()

    }
  )

})



