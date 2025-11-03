
# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

# set up
cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "EunomiaDefaultCohorts")
on.exit({rm(cohortTableHandler);gc()})

exportFolder <- file.path(tempdir(), "testCohortOverlaps")
dir.create(exportFolder, showWarnings = FALSE)
on.exit({unlink(exportFolder, recursive = TRUE)})

analysisSettings <- list(
  cohortIds = c(1, 2),
  minCellCount = 1
)

# function
pathToResultsDatabase <- execute_CohortOverlaps(
  exportFolder = exportFolder,
  cohortTableHandler = cohortTableHandler,
  analysisSettings = analysisSettings
)

analysisResults <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_resultsVisualisation_CohortsOverlaps_ui("test")
  ),
  function(input,output,session){
    mod_resultsVisualisation_CohortsOverlaps_server("test",analysisResults)
  },
  options = list(launch.browser=TRUE)
)

app

# run full app --------------------------------------------------------------
devtools::load_all(".")

pathToCO2AnalysisModulesConfigYalm  <-  testthat::test_path("config/CO2AnalysisModulesConfig.yml")
CO2AnalysisModulesConfig <- yaml::read_yaml(pathToCO2AnalysisModulesConfigYalm)
options = list(launch.browser=FALSE, port = 8561)

browseURL(paste0("http://localhost:8561/?analysisType=cohortOverlaps&pathToResultsDatabase=", pathToResultsDatabase))
run_app(CO2AnalysisModulesConfig, options = options)


