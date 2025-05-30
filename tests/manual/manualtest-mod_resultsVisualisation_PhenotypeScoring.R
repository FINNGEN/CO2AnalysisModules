# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

# set up
cohortTableHandler <-
  helper_createNewCohortTableHandler(addCohorts = "DiabetesSyntheaCohorts")

exportFolder <- file.path(tempdir(), "testCodeWAS")
dir.create(exportFolder, showWarnings = FALSE)

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

analysisResults <-
  duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)


# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_resultsVisualisation_PhenotypeScoring_ui("test")
  ),
  function(input, output, session) {
    mod_resultsVisualisation_PhenotypeScoring_server("test", analysisResults)
  },
  options = list(launch.browser = TRUE)
)

app

# run full app --------------------------------------------------------------
devtools::load_all(".")

pathToCO2AnalysisModulesConfigYalm <- testthat::test_path("config/atlasDemo_CO2AnalysisModulesConfig.yml")
CO2AnalysisModulesConfig <- yaml::read_yaml(pathToCO2AnalysisModulesConfigYalm)
options <- list(launch.browser = FALSE, port = 8561)

browseURL(paste0("http://localhost:8561/?analysisType=codeWAS&pathToResultsDatabase=", pathToResultsDatabase))
run_app(CO2AnalysisModulesConfig, options = options)
