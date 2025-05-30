#
# With covariates
#

test_that("executePhenotypeScoring works", {
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "DiabetesSyntheaCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir('testPhenotypeScoring')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 0,
    analysisIds = c(141, 342)
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_PhenotypeScoring(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_PhenotypeScoring(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  PhenotypeScoringResults <-
    analysisResults  |> dplyr::tbl("covariatesPerPerson")  |> dplyr::collect()
  PhenotypeScoringResults |> 
  dplyr::count(covariateId)  |> nrow() |> expect_gt(0)


  

})



test_that(".extractCovariatesPerPerson works", {

  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "DiabetesSyntheaCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  covariatesTable <- tibble::tribble(
    ~analysisId, ~domainId,    ~covariateId,  ~conceptId, ~isBinary, ~isSourceConcept, ~nCasesYes,
          141,   "Condition",   4214746141,     4214746,    TRUE,        TRUE,              65,
          141,   "Condition",   4322737141,     4322737,    TRUE,        TRUE,              88,
          141,   "Condition",   40274283141,    40274283,   TRUE,        TRUE,             157,
          342,   "Drug",        21600973342,    21600973,   TRUE,       FALSE,             27,
          342,   "Drug",        1123893342,     1123893,    TRUE,       FALSE,             19,
          342,   "Drug",        21601133342,    21601133,   TRUE,       FALSE,             54
  )

  covariatesPerPerson <- .extractCovariatesPerPerson(
    cohortTableHandler = cohortTableHandler,
    cohortId = 1,
    covariatesTable = covariatesTable
  )


  covariatesPerPerson  |> 
  dplyr::filter(value > 0) |>
  dplyr::count(covariateId) 
    
})
