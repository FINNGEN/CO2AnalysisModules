
test_that("execute_CohortOverlaps works", {

  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({rm(cohortTableHandler);gc()})

  exportFolder <- withr::local_tempdir('testCohortOverlaps')

  analysisSettings <- list(
    cohortIds = c(1, 2),
    minCellCount = 1
  )

  # function
  pathToResultsDatabase <- execute_CohortOverlaps(
    exportFolder = exportFolder,
    cohortTableHandler = cohortTableHandler,
    analysisSettings = analysisSettings
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CohortOverlaps(pathToResultsDatabase) |> expect_true()

})
