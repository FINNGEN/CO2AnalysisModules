test_that("execute_SubjectsLongitudinalData works", {

  skip_if_not(testingDatabase == "AtlasDevelopment-DBI")
  
  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({rm(cohortTableHandler);gc()})
  
  exportFolder <- withr::local_tempdir('testSubjectsLongitudinalData')
  
  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    nSubjects = 1000,
    seed = 1234, 
    prevalenceTable = "atlas-development-270609.sandbox_tools_r12.code_prevalence_stratified_r12_v1"
  )
  
  # function
  pathToResultsDatabase <- execute_SubjectsLongitudinalData(
    exportFolder = exportFolder,
    cohortTableHandler = cohortTableHandler,
    analysisSettings = analysisSettings
  )
  
  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_SubjectsLongitudinalData(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  analysisResults  |> dplyr::tbl("events")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  analysisResults  |> dplyr::tbl("measurements")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  analysisResults  |> dplyr::tbl("drugs")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  analysisResults  |> dplyr::tbl("eras")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  analysisResults  |> dplyr::tbl("concept_ancestor")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  analysisResults  |> dplyr::tbl("concept")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  analysisResults  |> dplyr::tbl("prevalence")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  analysisResults  |> dplyr::tbl("analysisInfo")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  
}) 

test_that("execute_SubjectsLongitudinalData works no prevalence table", {

  skip_if_not(testingDatabase == "Eunomia-GiBleed")
  
  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({rm(cohortTableHandler);gc()})
  
  exportFolder <- withr::local_tempdir('testSubjectsLongitudinalData')
  
  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = NULL,
    nSubjects = 100,
    seed = 123, 
    prevalenceTable = NULL
  )
  
  # function
  pathToResultsDatabase <- execute_SubjectsLongitudinalData(
    exportFolder = exportFolder,
    cohortTableHandler = cohortTableHandler,
    analysisSettings = analysisSettings
  )
  
  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_SubjectsLongitudinalData(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  analysisResults  |> dplyr::tbl("events")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  analysisResults  |> dplyr::tbl("measurements")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_equal(0)
  analysisResults  |> dplyr::tbl("drugs")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  analysisResults  |> dplyr::tbl("eras")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  analysisResults  |> dplyr::tbl("concept_ancestor")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  analysisResults  |> dplyr::tbl("concept")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  analysisResults  |> dplyr::tbl("prevalence")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_equal(0)
  analysisResults  |> dplyr::tbl("analysisInfo")  |> dplyr::count()  |> dplyr::pull(n) |>  expect_gt(0)
  
  
}) 
