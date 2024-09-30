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

  exportFolder <- withr::local_tempdir('testGWAS')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    phenotype = "FRACTUREFRACTURECONTROLS",
    description = "Cases-cohort: fracture; Controls-cohort: fracture-controls (db: Eunomia GiBleed)",
    analysisType = "additive",
    release = "Regenie"
  )

  # function
  expect_error({
    execute_GWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  },"SANDBOX_TOKEN is not set")

  withr::with_envvar(
    new = c("SANDBOX_TOKEN" = "dddddddd"),
    code = {
      expect_error({
        execute_GWAS(
          exportFolder = exportFolder,
          cohortTableHandler = cohortTableHandler,
          analysisSettings = analysisSettings
        )
      },"Could not resolve host:")
    }
  )

  withr::with_envvar(
    new = c("SANDBOX_TOKEN" = "test"),
    code = {
        result <- execute_GWAS(
          exportFolder = exportFolder,
          cohortTableHandler = cohortTableHandler,
          analysisSettings = analysisSettings
        )
        result$status |> expect_equal(FALSE)
    }
  )

})

