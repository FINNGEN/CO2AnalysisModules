
test_that("executeTimeCodeWAS works", {

  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasAsthmaCohortsMatched")
  withr::defer({rm(cohortTableHandler);gc()})

  exportFolder <- withr::local_tempdir('testTimeCodeWAS')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    analysisIds = c(101, 102, 141, 204, 601, 641, 301, 341, 404, 906, 701, 741, 801, 841, 501, 541),
    temporalStartDays = c(   -365*2, -365*1, 0,     1,   365+1 ),
    temporalEndDays =   c( -365*1-1,     -1, 0, 365*1,   365*2)
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_timeCodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )


  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_timeCodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  timeCodeWASResults <- analysisResults  |> dplyr::tbl("timeCodewasResults")  |> dplyr::collect()
  # last 3 digits of covariate_it are 101
  timeCodeWASResults |> dplyr::filter(covariateId %% 1000 == 101)   |> nrow() |> expect_gt(0)
})


test_that("executeTimeCodeWAS works for cohort start date outside of observation paeriod", {

  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasAsthmaCohortsMatched")
  withr::defer({rm(cohortTableHandler);gc()})

  exportFolder <- withr::local_tempdir('testTimeCodeWAS')

  # change one person
  DatabaseConnector::executeSql(
    connection= cohortTableHandler$connectionHandler$getConnection(),
    sql = "UPDATE main.observation_period SET observation_period_start_date = '1984-07-26' WHERE person_id = 9" )

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    analysisIds = c(101, 102, 141, 204, 601, 641, 301, 341, 404, 906, 701, 741, 801, 841, 501, 541),
    temporalStartDays = c(   -365*2, -365*1, 0,     1,   365+1 ),
    temporalEndDays =   c( -365*1-1,     -1, 0, 365*1,   365*2)
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_timeCodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_timeCodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  timeCodeWASResults <- analysisResults  |> dplyr::tbl("timeCodewasResults")  |> dplyr::collect()
  # last 3 digits of covariate_it are 101
  timeCodeWASResults |> dplyr::filter(covariateId %% 1000 == 101)   |> nrow() |> expect_gt(0)
})


