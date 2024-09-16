
# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

# set up
cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohortsMatched")
on.exit({rm(cohortTableHandler);gc()})

exportFolder <- file.path(tempdir(), "testtimeCodeWAS")
dir.create(exportFolder, showWarnings = FALSE)
on.exit({unlink(exportFolder, recursive = TRUE)})

analysisSettings <- list(
  cohortIdCases = 1,
  cohortIdControls = 2,
  analysisIds = c(101, 102, 141, 204, 601, 641, 301, 341, 404, 906, 701, 741, 801, 841, 501, 541),
  temporalStartDays = c(   -365*2, -365*1, 0,     1,   365+1 ),
  temporalEndDays =   c( -365*1-1,     -1, 0, 365*1,   365*2)
)


# function
pathToResultsDatabase <- execute_timeCodeWAS(
  exportFolder = exportFolder,
  cohortTableHandler = cohortTableHandler,
  analysisSettings = analysisSettings
)

analysisResults <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
      mod_resultsVisualisation_TimeCodeWAS_ui("test")
  ),
  function(input,output,session){
    mod_resultsVisualisation_TimeCodeWAS_server("test",analysisResults)
  },
  options = list(launch.browser=TRUE)
)

app

# run full app --------------------------------------------------------------
devtools::load_all(".")

pathToCO2AnalysisModulesConfigYalm  <-  testthat::test_path("config/atlasDemo_CO2AnalysisModulesConfig.yml")
CO2AnalysisModulesConfig <- yaml::read_yaml(pathToCO2AnalysisModulesConfigYalm)
options = list(launch.browser=FALSE, port = 5907)

browseURL(paste0("http://localhost:5907/?pathToResultsDatabase=", pathToResultsDatabase))
run_app(CO2AnalysisModulesConfig, options = options)


