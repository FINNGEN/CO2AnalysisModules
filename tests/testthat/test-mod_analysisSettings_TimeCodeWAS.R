
test_that("mod_analysisSettings_timetimeCodeWAS works", {

  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohortsMatched")
  withr::defer({rm(cohortTableHandler);gc()})

  r_connectionHandler <- shiny::reactiveValues(
    cohortTableHandler = cohortTableHandler,
    hasChangeCounter = 0
  )

  # run module
  shiny::testServer(
    mod_analysisSettings_timeCodeWAS_server,
    args = list(
      id = "test",
      r_connectionHandler = r_connectionHandler
    ),
    {
      #
      session$setInputs(
        selectCaseCohort_pickerInput = 1,
        selectControlCohort_pickerInput = 2,
        features_pickerInput = c(101, 141, 1, 2, 402, 702, 41),
        temporalStartDays = c(-1826, -365, 0, 1),
        temporalEndDays = c(-365, 0, 1, 366),
        minCellCount_numericInput = 1)

      analysisSettings <- rf_analysisSettings()

      analysisSettings |> assertAnalysisSettings_timeCodeWAS() |> expect_no_error()

      analysisSettings |> expect_equal(
        list(
          cohortIdCases = 1,
          cohortIdControls = 2,
          analysisIds = c(101, 141, 1, 2, 402, 702, 41),
          temporalStartDays = 0,
          temporalEndDays = 0
        )
      )

      output$info_text |> expect_match("No subjects overlap between case and control cohorts")
      output$info_text |> expect_match("There is a significant difference in year of birth distribution between case and control cohorts")

      #
      session$setInputs(
        selectCaseCohort_pickerInput = 1,
        selectControlCohort_pickerInput = 2001,
        features_pickerInput = c(101, 141, 1, 2, 402, 702, 41),
        temporalStartDays = c(-1826, -365, 0, 1),
        temporalEndDays = c(-365, 0, 1, 366),
        minCellCount_numericInput = 1)

      analysisSettings <- rf_analysisSettings()

      analysisSettings |> assertAnalysisSettings_timeCodeWAS() |> expect_no_error()

      analysisSettings |> expect_equal(
        list(
          cohortIdCases = 1,
          cohortIdControls = 2001,
          analysisIds = c(101, 141, 1, 2, 402, 702, 41),
          temporalStartDays = 0,
          temporalEndDays = 0
        )
      )

      output$info_text |> expect_match("No subjects overlap between case and control cohorts")
      output$info_text |> expect_no_match("There is a significant difference in sex distribution between case and control cohorts")

    }
  )

})



