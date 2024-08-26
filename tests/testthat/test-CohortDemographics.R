
test_that("execute_CohortDemographics works", {

  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({rm(cohortTableHandler);gc()})

  exportFolder <- withr::local_tempdir('testCohortOverlaps')

  analysisSettings <- list(
    cohortIds = c(1, 2),
    referenceYears = c("cohort_start_date", "cohort_end_date", "birth_datetime"),
    groupBy = c("calendarYear", "ageGroup", "gender"),
    minCellCount = 1
  )

  # function
  pathToResultsDatabase <- execute_CohortDemographics(
    exportFolder = exportFolder,
    cohortTableHandler = cohortTableHandler,
    analysisSettings = analysisSettings
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CohortDemographics(pathToResultsDatabase) |> expect_true()

})
