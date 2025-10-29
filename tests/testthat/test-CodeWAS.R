#
# With covariates
#

test_that("executeCodeWAS works", {
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir('testCodeWAS')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    analysisIds = c(101, 141, 1, 2, 402, 702, 41),
    covariatesIds = c(8507001, 1041),
    minCellCount = 1
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_CodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  codeWASResults <-
    analysisResults  |> dplyr::tbl("codewasResults")  |> dplyr::collect()
  codeWASResults |> dplyr::filter(covariateId == 1002)  |> nrow() |> expect_equal(1)
  codeWASResults |> dplyr::filter(covariateId == 1041)  |> nrow() |> expect_equal(0)
  codeWASResults |> dplyr::filter(covariateId == 8507001)  |> nrow() |> expect_equal(0)
  codeWASResults |> dplyr::filter(stringr::str_detect(covariateId, "101$"))  |> nrow() |> expect_gt(0)

})


test_that("executeCodeWAS warnings with cohort overlap", {
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir('HadesExtrasAsthmaCohorts')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    analysisIds = c(101, 141, 1, 2, 402, 702, 41),
    covariatesIds = c(8507001, 1041),
    minCellCount = 1
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_CodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  codeWASResults <-
    analysisResults  |> dplyr::tbl("codewasResults")  |> dplyr::collect()
  codeWASResults |> dplyr::filter(covariateId == 1002)  |> nrow() |> expect_equal(1)
  codeWASResults |> dplyr::filter(covariateId == 1041)  |> nrow() |> expect_equal(0)
  codeWASResults |> dplyr::filter(covariateId == 8507001)  |> nrow() |> expect_equal(0)
  codeWASResults |> dplyr::filter(stringr::str_detect(covariateId, "101$"))  |> nrow() |> expect_gt(0)

})


test_that("executeCodeWAS works spliting in chuncs", {
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir('testCodeWAS')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    analysisIds = c(101, 141, 1, 2, 402, 702, 41),
    covariatesIds = c(8507001, 1041),
    minCellCount = 1,
    chunksSizeNOutcomes =  100
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_CodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  codeWASResults <-
    analysisResults  |> dplyr::tbl("codewasResults")  |> dplyr::collect()
  codeWASResults |> dplyr::filter(covariateId == 1002)  |> nrow() |> expect_equal(1)
  codeWASResults |> dplyr::filter(covariateId == 1041)  |> nrow() |> expect_equal(0)
  codeWASResults |> dplyr::filter(covariateId == 8507001)  |> nrow() |> expect_equal(0)
  codeWASResults |> dplyr::filter(stringr::str_detect(covariateId, "101$"))  |> nrow() |> expect_gt(0)

})


test_that("executeCodeWAS works with regex cohorts", {
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir('testCodeWAS')


  analysisRegexTibble = tibble::tribble(
        ~analysisId, ~analysisName, ~analysisRegex,
        999, "Endpoints", "^(?!.*\\[CohortLibrary\\]).*_case$",
        998, "CohortLibrary", ".*\\[CohortLibrary\\]"
  )

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    analysisIds = c(101, 141, 1, 2, 402, 702, 41, 999, 998),
    covariatesIds = NULL,
    minCellCount = 1,
    analysisRegexTibble = analysisRegexTibble
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_CodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  skip_if(testingDatabase != "Eunomia-GiBleed", "Skip test, it is only for Eunomia-GiBleed")

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  codeWASResults <-
    analysisResults  |> dplyr::tbl("codewasResults")  |> dplyr::collect()
  codeWASResults |> dplyr::filter(covariateId %% 1000 == 999)  |> nrow() |> expect_equal(2)
  codeWASResults |> dplyr::filter(covariateId %% 1000 == 998)  |> nrow() |> expect_equal(2)

  covariateRef <-
    analysisResults  |> dplyr::tbl("covariateRef")  |> dplyr::collect()
  covariateRef |> dplyr::filter(analysisId == 999) |> dplyr::pull(covariateName) |> 
    expect_equal(c("Cohort of patients diagnosed with diabetes", "Cohort of patients diagnosed with hypertension"))
  covariateRef |> dplyr::filter(analysisId == 999) |> dplyr::pull(vocabularyId) |> 
    expect_equal(c("Endpoints", "Endpoints"))
  covariateRef |> dplyr::filter(analysisId == 998) |> dplyr::pull(covariateName) |> 
    expect_equal(c("Cohort of patients diagnosed with hypertension", "Cohort of patients diagnosed with obesity"))
  covariateRef |> dplyr::filter(analysisId == 998) |> dplyr::pull(vocabularyId) |> 
    expect_equal(c("CohortLibrary", "CohortLibrary"))

  analysisRef <-
    analysisResults  |> dplyr::tbl("analysisRef")  |> dplyr::collect()
  analysisRef |> dplyr::filter(analysisId == 999) |> dplyr::pull(analysisName)  |> 
    expect_equal("Endpoints")
  analysisRef |> dplyr::filter(analysisId == 998) |> dplyr::pull(analysisName)  |> 
    expect_equal("CohortLibrary")
})

test_that("executeCodeWAS works with regex cohorts no selected", {
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir('testCodeWAS')


  analysisRegexTibble = tibble::tribble(
        ~analysisId, ~analysisName, ~analysisRegex,
        999, "Endpoints", "^(?!.*\\[CohortLibrary\\]).*_case$",
        998, "CohortLibrary", ".*\\[CohortLibrary\\]"
  )

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    analysisIds = c(101, 141, 1, 2, 402, 702, 41),
    covariatesIds = NULL,
    minCellCount = 1,
    analysisRegexTibble = analysisRegexTibble
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_CodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  skip_if(testingDatabase != "Eunomia-GiBleed", "Skip test, it is only for Eunomia-GiBleed")

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  codeWASResults <-
    analysisResults  |> dplyr::tbl("codewasResults")  |> dplyr::collect()
  codeWASResults |> dplyr::filter(covariateId %% 1000 == 999)  |> nrow() |> expect_equal(0)
  codeWASResults |> dplyr::filter(covariateId %% 1000 == 998)  |> nrow() |> expect_equal(0)

  analysisRef <-
    analysisResults  |> dplyr::tbl("analysisRef")  |> dplyr::collect()
  analysisRef |> dplyr::filter(analysisId == 999) |> nrow() |> expect_equal(0)
  analysisRef |> dplyr::filter(analysisId == 998) |> nrow() |> expect_equal(0)

  covariateRef <-
    analysisResults  |> dplyr::tbl("covariateRef")  |> dplyr::collect()
})



# test_that("executeCodeWAS works for big size", {
#   library(ParallelLogger)
#   clearLoggers()
#   logger <- createLogger(name = "SIMPLE",
#                          threshold = "INFO",
#                          appenders = list(createConsoleAppender(layout = layoutTimestamp)))
#
#   registerLogger(logger)
#
#   if(testSelectedConfiguration$database$databaseName != "bigquery500k"){
#     skip("Skip test, it is only for bigquery500k")
#   }
#
#   cohortTableHandler <- helper_createNewCohortTableHandler()
#   on.exit({rm(cohortTableHandler);gc()})
#
#
#   cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
#     settingsFileName = here::here("inst/testdata/fracture/Cohorts.csv"),
#     jsonFolder = here::here("inst/testdata/fracture/cohorts"),
#     sqlFolder = here::here("inst/testdata/fracture/sql/sql_server"),
#     cohortFileNameFormat = "%s",
#     cohortFileNameValue = c("cohortId"),
#     subsetJsonFolder = here::here("inst/testdata/fracture/cohort_subset_definitions/"),
#     #packageName = "HadesExtras",
#     verbose = FALSE
#   )
#
#   cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)
#   cohortTableHandler$getCohortCounts()
#
#   analysisIds  <- c(101, 1, 41)
#
#   exportFolder <- file.path(tempdir(), "CodeWAS")
#   dir.create(exportFolder, showWarnings = FALSE)
#   on.exit({unlink(exportFolder, recursive = TRUE);gc()})
#
#   startTime <- Sys.time()
#   suppressWarnings(
#     executeCodeWAS(
#       exportFolder = exportFolder,
#       cohortTableHandler = cohortTableHandler,
#       cohortIdCases = 1,
#       cohortIdControls = 2,
#       analysisIds = analysisIds,
#       covariatesIds = c(8507001, 1041),
#       minCellCount = 1,
#       cores = 7
#     )
#   )
#   print(Sys.time() - startTime)
#
#
#   expect_true(file.exists(file.path(exportFolder, "codewas_results.csv")))
#
#   codeWASResults <- read.csv(file.path(exportFolder, "codewas_results.csv"))  |> tibble::as_tibble()
#   codeWASResults |> dplyr::filter(covariateId == 1002)  |> nrow() |> expect_equal(1)
#   codeWASResults |> dplyr::filter(covariateId == 1041)  |> nrow() |> expect_equal(0)
#   codeWASResults |> dplyr::filter(covariateId == 8507001)  |> nrow() |> expect_equal(0)
#   codeWASResults |> dplyr::filter( stringr::str_detect(covariateId, "101$"))  |> nrow() |> expect_gt(0)
#
#
# })


#
# Without covariates
#


test_that("executeCodeWAS works with no covariates", {
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir('testCodeWAS')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    analysisIds = c(101, 141, 1, 2, 402, 702, 41),
    covariatesIds = NULL,
    minCellCount = 1
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_CodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  codeWASResults <-
    analysisResults  |> dplyr::tbl("codewasResults")  |> dplyr::collect()
  codeWASResults |> dplyr::filter(covariateId == 1002)  |> nrow() |> expect_equal(1)
  codeWASResults |> dplyr::filter(covariateId == 1041)  |> nrow() |> expect_equal(1)
  codeWASResults |> dplyr::filter(covariateId == 8507001)   |> nrow() |> expect_equal(1)

  codeWASResults |> dplyr::filter(modelType != "No test, not enough samples")  |> dplyr::filter(is.na(pValue))  |> nrow() |> expect_equal(0)
  codeWASResults |> dplyr::filter(modelType != "No test, not enough samples")  |> dplyr::filter(is.na(oddsRatio))  |> nrow() |> expect_equal(0)

})


test_that("executeCodeWAS works when n multiple events per subject", {
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasAsthmaCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  # Match to sex and bday, match ratio 10
  subsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "",
    definitionId = 1,
    subsetOperators = list(
      HadesExtras::createMatchingSubset(
        matchToCohortId = 1,
        matchRatio = 10,
        matchSex = TRUE,
        matchBirthYear = TRUE,
        matchCohortStartDateWithInDuration = FALSE,
        newCohortStartDate = "asMatch",
        newCohortEndDate = "keep"
      )
    )
  )

  cohortDefinitionSetWithSubsetDef <-
    cohortTableHandler$cohortDefinitionSet |>
    CohortGenerator::addCohortSubsetDefinition(subsetDef, targetCohortIds = 2)

  suppressWarnings(cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSetWithSubsetDef))

  exportFolder <- withr::local_tempdir('testCodeWAS')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2001,
    analysisIds = c(101, 141, 1, 2, 402, 702, 41),
    covariatesIds = NULL,
    minCellCount = 1
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_CodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  codeWASResults <-
    analysisResults  |> dplyr::tbl("codewasResults")  |> dplyr::collect()
  codeWASResults |> dplyr::filter(covariateId == 1002)  |> nrow() |> expect_equal(1)
  codeWASResults |> dplyr::filter(covariateId == 1041)  |> nrow() |> expect_equal(1)
  codeWASResults |> dplyr::filter(covariateId == 8507001)  |> nrow() |> expect_equal(1)
  codeWASResults |> dplyr::filter(stringr::str_detect(covariateId, "101$"))  |> nrow() |> expect_gt(0)

})



test_that("executeCodeWAS works to get lab values", {
  skip_if(testingDatabase != "Eunomia-FinnGen",
          "Skip test, it is only for Eunomia-FinnGen")

  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir('testCodeWAS')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 2,
    analysisIds = c(101, 141, 1, 2, 402, 701, 702, 41),
    covariatesIds = NULL,
    minCellCount = 1
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_CodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

  codeWASResults <-
    analysisResults  |> dplyr::tbl("codewasResults")  |> dplyr::collect()

  codeWASResults |> dplyr::filter(covariateId %% 1000 == 702)  |> nrow() |> expect_gt(0)

  (codeWASResults |> dplyr::filter(modelType == "No test, not enough samples")  |> dplyr::filter(is.na(pValue))  |> nrow() ==
   codeWASResults |> dplyr::filter(modelType == "No test, not enough samples")  |> nrow()) |>
    expect_true()
  (codeWASResults |> dplyr::filter(modelType == "No test, not enough samples")  |> dplyr::filter(is.na(oddsRatio))  |> nrow() ==
   codeWASResults |> dplyr::filter(modelType == "No test, not enough samples")  |> nrow()) |>
    expect_true()

})



test_that("executeCodeWAS works with 0 as control cohort", {
  
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir('testCodeWAS')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 0,
    analysisIds = c(101, 141, 1, 2, 402, 701, 702, 41),
    covariatesIds = NULL,
    minCellCount = 1
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_CodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

cohortsInfo <-
    analysisResults  |> dplyr::tbl("cohortsInfo")  |> dplyr::collect()
  cohortsInfo |> nrow() |> expect_equal(4)
  cohortsInfo |> dplyr::filter(cohortId == 3) |> pull(shortName) |> expect_equal("ALL\u2229C1")
  cohortsInfo |> dplyr::filter(cohortId == 1) |> pull(use) |> expect_equal('cases')
  cohortsInfo |> dplyr::filter(cohortId == 3001) |> pull(shortName) |> expect_equal("MxALL\u2229C1")
  cohortsInfo |> dplyr::filter(cohortId == 3001) |> pull(use) |> expect_equal('controls')

  # test that the cohorts have been deleted
  cohortTableHandler$getCohortCounts() |> pull(cohortId) |> expect_equal(c(1, 2))

})



test_that("executeCodeWAS works with only binary covariates", {
  
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir('testCodeWAS')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 0,
    analysisIds = c(101, 102, 141),
    covariatesIds = NULL,
    minCellCount = 1
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_CodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

cohortsInfo <-
    analysisResults  |> dplyr::tbl("cohortsInfo")  |> dplyr::collect()
  cohortsInfo |> nrow() |> expect_equal(4)
  cohortsInfo |> dplyr::filter(cohortId == 3) |> pull(shortName) |> expect_equal("ALL\u2229C1")
  cohortsInfo |> dplyr::filter(cohortId == 1) |> pull(use) |> expect_equal('cases')
  cohortsInfo |> dplyr::filter(cohortId == 3001) |> pull(shortName) |> expect_equal("MxALL\u2229C1")
  cohortsInfo |> dplyr::filter(cohortId == 3001) |> pull(use) |> expect_equal('controls')

  # test that the cohorts have been deleted
  cohortTableHandler$getCohortCounts() |> pull(cohortId) |> expect_equal(c(1, 2))

})



test_that("executeCodeWAS works with ATC groups and DDDs", {
  skip_if_not(testingDatabase  |> stringr::str_starts("AtlasDevelopment"),
          "Skip test, it is only for AtlasDevelopment databases")
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasAsthmaCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir('testCodeWAS')

  analysisSettings <- list(
    cohortIdCases = 1,
    cohortIdControls = 0,
    analysisIds = c(342, 343),
    covariatesIds = NULL,
    minCellCount = 1
  )

  # function
  suppressWarnings(
    pathToResultsDatabase <- execute_CodeWAS(
      exportFolder = exportFolder,
      cohortTableHandler = cohortTableHandler,
      analysisSettings = analysisSettings
    )
  )

  # test
  expect_true(file.exists(pathToResultsDatabase))
  checkResults_CodeWAS(pathToResultsDatabase) |> expect_true()

  analysisResults <-
    duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

cohortsInfo <-
    analysisResults  |> dplyr::tbl("cohortsInfo")  |> dplyr::collect()
  cohortsInfo |> nrow() |> expect_equal(4)
  cohortsInfo |> dplyr::filter(cohortId == 3) |> pull(shortName) |> expect_equal("ALL\u2229C1")
  cohortsInfo |> dplyr::filter(cohortId == 1) |> pull(use) |> expect_equal('cases')
  cohortsInfo |> dplyr::filter(cohortId == 3001) |> pull(shortName) |> expect_equal("MxALL\u2229C1")
  cohortsInfo |> dplyr::filter(cohortId == 3001) |> pull(use) |> expect_equal('controls')

  # test that the cohorts have been deleted
  cohortTableHandler$getCohortCounts() |> pull(cohortId) |> expect_equal(c(1, 2))

  codewasResults <-
    analysisResults  |> dplyr::tbl("codewasResults")  |> dplyr::collect()
  if (Sys.getenv("HADESEXTAS_TESTING_ENVIRONMENT") == "AtlasDevelopment-DBI") {
    codewasResults |> dplyr::filter(covariateId %% 1000 == 342) |> nrow() |> expect_gt(0)
    codewasResults |> dplyr::filter(covariateId %% 1000 == 343) |> nrow() |> expect_gt(0)
  }

  analysisRef <-
    analysisResults  |> dplyr::tbl("analysisRef")  |> dplyr::collect()
  analysisRef |> nrow() |> expect_equal(2)
  analysisRef |> dplyr::filter(analysisId == 342) |> pull(analysisName) |> expect_equal("ATCgroups")
  analysisRef |> dplyr::filter(analysisId == 343) |> pull(analysisName) |> expect_equal("DDDATCgroups")

})
