#
# With covariates
#

test_that("executeGWAS error if no token", {
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir("testGWAS")

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    phenotype = "FRACTUREFRACTURECONTROLS",
    description = "Cases-cohort: fracture; Controls-cohort: fracture-controls (db: Eunomia GiBleed)",
    analysisType = "additive",
    release = "Regenie12",
    connectionSandboxAPI = createSandboxAPIConnection(
      base_url = "https://sandbox-api.finngen.fi/api/v1/",
      token = "1234567890"
    )
  )

  # function
  expect_error(
    {
      execute_GWAS(
        exportFolder = exportFolder,
        cohortTableHandler = cohortTableHandler,
        analysisSettings = analysisSettings
      )
    },
    "GWAS run failedConnection in connection_sandboxAPI"
  )
})
