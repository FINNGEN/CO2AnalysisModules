
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
  analysisIds = c(
    101, 102, 141, 204,
    601, 641,
    301, 341, 404,
    701, 702, 703, 741,
    801, 841,
    501, 541,
    910, 911 ),
  # temporalStartDays = c(   -365*2, -365*1, 0,     1,   365+1 ),
  # temporalEndDays =   c( -365*1-1,     -1, 0, 365*1,   365*2)
  temporalStartDays = c(-609, -304, -152, -61, 0, 1, 62, 153, 305),
  temporalEndDays =   c(-305, -153,  -62, -1, -0, 61, 152, 304, 609 )
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
options = list(launch.browser=FALSE, port = 8561)

browseURL(paste0("http://localhost:8561/?analysisType=timeCodeWAS&pathToResultsDatabase=", pathToResultsDatabase))
run_app(CO2AnalysisModulesConfig, options = options)


