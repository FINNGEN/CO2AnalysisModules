
test_that("mod_analysisSettings_CodeWAS works", {

  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({rm(cohortTableHandler);gc()})

  r_connectionHandler <- shiny::reactiveValues(
    cohortTableHandler = cohortTableHandler,
    hasChangeCounter = 0
  )

  # run module
  shiny::testServer(
    mod_analysisSettings_codeWAS_server,
    args = list(
      id = "test",
      r_connectionHandler = r_connectionHandler
    ),
    {
      # "aggregated"
      session$setInputs(
        selectCaseCohort_pickerInput = 1,
        selectControlCohort_pickerInput = 2,
        features_pickerInput = c(101, 141, 1, 2, 402, 702, 41),
        statistics_type_option =  "aggregated",
        controlSex_checkboxInput = TRUE,
        controlYearOfBirth_checkboxInput = TRUE,
        minCellCount_numericInput = 1)

      analysisSettings <- rf_analysisSettings()

      analysisSettings |> assertAnalysisSettings_CodeWAS() |> expect_no_error()
      analysisSettings |> expect_equal(
        list(
          cohortIdCases = 1,
          cohortIdControls = 2,
          analysisIds = c(101, 141, 1, 2, 402, 702, 41),
          covariatesIds = NULL,
          minCellCount = 1,
          chunksSizeNOutcomes = 5000,
          cores = 1
        )
      )

      output$info_text |> expect_match("No subjects overlap between case and control cohorts")
      output$info_text |> expect_match("There is a significant difference in sex distribution between case and control cohorts")
      output$info_text |> expect_match("There is a significant difference in year of birth distribution between case and control cohorts")

      # check "full" gets covariates
      session$setInputs(
        selectCaseCohort_pickerInput = 1,
        selectControlCohort_pickerInput = 2,
        features_pickerInput = c(101, 141, 1, 2, 402, 702, 41),
        statistics_type_option =  "full",
        controlSex_checkboxInput = TRUE,
        controlYearOfBirth_checkboxInput = TRUE,
        minCellCount_numericInput = 1)

      analysisSettings <- rf_analysisSettings()

      analysisSettings |> assertAnalysisSettings_CodeWAS() |> expect_no_error()
      analysisSettings |> expect_equal(
        list(
          cohortIdCases = 1,
          cohortIdControls = 2,
          analysisIds = c(101, 141, 1, 2, 402, 702, 41),
          covariatesIds = c(8507001, 1041),
          minCellCount = 1,
          chunksSizeNOutcomes = 5000,
          cores = 1
        )
      )

      output$info_text |> expect_match("No subjects overlap between case and control cohorts")
      output$info_text |> expect_no_match("There is a significant difference in sex distribution between case and control cohorts")
      output$info_text |> expect_no_match("There is a significant difference in year of birth distribution between case and control cohorts")

    }
  )

})



