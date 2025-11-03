
test_that("executeTimeCodeWAS works", {

  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasAsthmaCohortsMatched")
  withr::defer({rm(cohortTableHandler);gc()})

  exportFolder <- withr::local_tempdir('testTimeCodeWAS')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    analysisIds = c(101, 301, 701, 702, 741, 801, 841),
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

  skip_if(testingDatabase != "Eunomia-FinnGen",
          "Skip test, it is only for Eunomia-FinnGen")
  timeCodeWASResults |> dplyr::filter(covariateId %% 1000 == 702)   |> nrow() |> expect_gt(0)

})



test_that("executeTimeCodeWAS works with 0 as control cohort", {

  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({rm(cohortTableHandler);gc()})

  exportFolder <- withr::local_tempdir('testTimeCodeWAS')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 0,
    analysisIds = c(101, 301, 701, 702, 741, 801, 841),
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

  cohortsInfo <-
    analysisResults  |> dplyr::tbl("cohortsInfo")  |> dplyr::collect()
  cohortsInfo |> nrow() |> expect_equal(4)
  cohortsInfo |> dplyr::filter(cohortId == 3) |> pull(shortName) |> expect_equal("ALL\u2229frac")
  cohortsInfo |> dplyr::filter(cohortId == 1) |> pull(use) |> expect_equal('cases')
  cohortsInfo |> dplyr::filter(cohortId == 3001) |> pull(shortName) |> expect_equal("MxALL\u2229frac")
  cohortsInfo |> dplyr::filter(cohortId == 3001) |> pull(use) |> expect_equal('controls')

  # test that the cohorts have been deleted
  cohortTableHandler$getCohortCounts() |> pull(cohortId) |> expect_equal(c(1, 2))

})

