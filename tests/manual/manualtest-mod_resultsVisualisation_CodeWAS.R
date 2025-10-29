
# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

# set up
cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohortsMatched")
on.exit({rm(cohortTableHandler);gc()})

exportFolder <- file.path(tempdir(), "testCodeWAS")
dir.create(exportFolder, showWarnings = FALSE)
on.exit({unlink(exportFolder, recursive = TRUE)})

analysisRegexTibble = tibble::tribble(
        ~analysisId, ~analysisName, ~analysisRegex,
        999, "Endpoints", "^(?!.*\\[CohortLibrary\\]).*_case$",
        998, "CohortLibrary", ".*\\[CohortLibrary\\]"
  )

analysisSettings <- list(
  cohortIdCases = 1,
  cohortIdControls = 2,
  analysisIds = c(101, 141, 1, 2, 402, 701, 702, 41, 999, 998),
  covariatesIds = NULL,
  minCellCount = 1,
  analysisRegexTibble = analysisRegexTibble
)


# function
pathToResultsDatabase <- execute_CodeWAS(
  exportFolder = exportFolder,
  cohortTableHandler = cohortTableHandler,
  analysisSettings = analysisSettings
)

analysisResults <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
      mod_resultsVisualisation_CodeWAS_ui("test")
  ),
  function(input,output,session){
    mod_resultsVisualisation_CodeWAS_server("test",analysisResults)
  },
  options = list(launch.browser=TRUE)
)

app

# run full app --------------------------------------------------------------
devtools::load_all(".")

pathToCO2AnalysisModulesConfigYalm  <-  testthat::test_path("config/CO2AnalysisModulesConfig.yml")
CO2AnalysisModulesConfig <- yaml::read_yaml(pathToCO2AnalysisModulesConfigYalm)
options = list(launch.browser=FALSE, port = 8561)

browseURL(paste0("http://localhost:8561/?analysisType=codeWAS&pathToResultsDatabase=", pathToResultsDatabase))
run_app(CO2AnalysisModulesConfig, options = options)


