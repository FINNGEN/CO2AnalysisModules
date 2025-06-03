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

  exportFolder <- withr::local_tempdir("testPhenotypeScoring")

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

  covariatesPerPerson <- analysisResults |> dplyr::tbl("covariatesPerPerson")  |> dplyr::collect()
  gwasResults <- analysisResults |> dplyr::tbl("codewasResults") |> dplyr::collect()

  covariatesPerPerson |> dplyr::filter(is.na(value)) |> nrow() |> expect_equal(0)
  covariatesPerPerson |> dplyr::filter(is.na(covariateId)) |> nrow() |> expect_equal(0)
  covariatesPerPerson |> dplyr::filter(is.na(unit)) |> nrow() |> expect_equal(0)

  covariatesPerPerson |> dplyr::filter(value <= 0) |> 
  dplyr::mutate(a= covariateId%%1000)  |> dplyr::arrange(a)  |> View() 
  nrow() |> expect_equal(0)

  # same number of subjects in covariatesPerPerson and gwasResults
  covariatesPerPerson |>
  dplyr::count(covariateId)   |> 
  dplyr::left_join(gwasResults |> dplyr::select(covariateId, nCasesYes), by = "covariateId")  |> 
  dplyr::filter(nCasesYes != n) |> dplyr::mutate(a= covariateId%%1000)  |> dplyr::arrange(a)  |> View()
  nrow() |> 
  expect_equal(0)
 

 # discrepancy in analysis 141 is due to condition_occurrence.condition_end_date being null
 
 
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
    ~analysisId, ~domainId, ~covariateId, ~conceptId, ~isBinary, ~isSourceConcept, ~nCasesYes,
    141, "Condition", 4214746141, 4214746, TRUE, TRUE, 65,
    141, "Condition", 4322737141, 4322737, TRUE, TRUE, 88,
    141, "Condition", 40274283141, 40274283, TRUE, TRUE, 157,
    141, "Condition", 443601141, 443601, TRUE, TRUE, 47,
    342, "Drug", 21600973342, 21600973, TRUE, FALSE, 27,
    342, "Drug", 1123893342, 1123893, TRUE, FALSE, 19,
    342, "Drug", 21601133342, 21601133, TRUE, FALSE, 54,
    342, "Drug", 715899342, 715899, TRUE, FALSE, 4
  )

  covariatesPerPerson <- .extractCovariatesPerPerson(
    cohortTableHandler = cohortTableHandler,
    cohortId = 1,
    covariatesTable = covariatesTable
  )
    
  covariatesTable |>
  dplyr::select(analysisId, domainId, covariateId, conceptId, nCasesYes) |>
    dplyr::left_join(
      covariatesPerPerson |> dplyr::count(covariateId) , 
      by = "covariateId") 
})
