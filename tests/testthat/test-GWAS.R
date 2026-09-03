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
    continueWithFinemap = FALSE,
    release = "Regenie13",
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
    "Could not resolve host: sandbox-api.finngen.fi"
  )
})

test_that("Regenie standard pipeline input includes all defaults", {
  inputs <- CO2AnalysisModules:::.build_regenie_standard_pipeline_inputs(
    pheno_file = "SANDBOX_RED/CO2_temp/test/phenofile.tsv",
    phenotype_name = "TEST_PHENOTYPE",
    phenodescription_file = "SANDBOX_RED/CO2_temp/test/phenodescriptionfile.txt"
  )

  expect_identical(
    names(inputs),
    c(
      "regenie_unmod.pheno_file",
      "regenie_unmod.phenolist",
      "regenie_unmod.phenodescriptionlist",
      "regenie_unmod.test",
      "regenie_unmod.is_binary",
      "regenie_unmod.continue_with_finemap",
      "regenie_unmod.minmac",
      "regenie_unmod.covariates"
    )
  )
  expect_identical(
    inputs[["regenie_unmod.pheno_file"]],
    "SANDBOX_RED/CO2_temp/test/phenofile.tsv"
  )
  expect_identical(inputs[["regenie_unmod.phenolist"]], "TEST_PHENOTYPE")
  expect_identical(
    inputs[["regenie_unmod.phenodescriptionlist"]],
    "SANDBOX_RED/CO2_temp/test/phenodescriptionfile.txt"
  )
  expect_identical(inputs[["regenie_unmod.test"]], "additive")
  expect_identical(inputs[["regenie_unmod.is_binary"]], "TRUE")
  expect_identical(inputs[["regenie_unmod.continue_with_finemap"]], "TRUE")
  expect_identical(inputs[["regenie_unmod.minmac"]], "5")
  expect_identical(inputs[["regenie_unmod.covariates"]], CO2AnalysisModules:::.default_regenie_covariates())
})