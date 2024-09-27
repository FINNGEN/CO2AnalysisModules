
# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

# set up
cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
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

pathAboutModule <- system.file('modulesDocumentation/about_cohortOverlaps.md', package = "CO2AnalysisModules")

analysisType <- "dummy"
mod_resultsVisualisation_dummy_ui <- function(id){}
mod_resultsVisualisation_dummy_server <- function(id,analysisResults){
  shiny::moduleServer(id, function(input, output, session){})
}

timestamp  <- as.character(as.numeric(format(Sys.time(), "%d%m%Y%H%M%OS2"))*100)
logsFolder <- paste0(analysisType, "_", timestamp)
logshref  <- fcr_setUpLogger(logsFolder = logsFolder)

app <- shiny::shinyApp(
  shiny::fluidPage(
      mod_resultsVisualisation_ui("test", mod_resultsVisualisation_CohortsOverlaps_ui, pathAboutModule, "Title", logshref)
  ),
  function(input,output,session){
     mod_resultsVisualisation_server("test", mod_resultsVisualisation_CohortsOverlaps_server, analysisResults)
  },
  options = list(launch.browser=TRUE)
)

app
