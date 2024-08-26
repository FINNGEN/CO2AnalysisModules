
# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

# set up
cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "EunomiaDefaultCohorts")
on.exit({rm(cohortTableHandler);gc()})

exportFolder <- file.path(tempdir(), "testCohortDemographics")
dir.create(exportFolder, showWarnings = FALSE)
on.exit({unlink(exportFolder, recursive = TRUE)})

analysisSettings <- list(
  cohortIds = c(1, 2),
  referenceYears = c("cohort_start_date", "cohort_end_date", "birth_datetime"),
  groupBy = c("calendarYear", "ageGroup", "gender"),
  minCellCount = 1
)

# function
pathToResultsDatabase <- execute_CohortDemographics(
  exportFolder = exportFolder,
  cohortTableHandler = cohortTableHandler,
  analysisSettings = analysisSettings
)

analysisResults <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)

# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
      mod_resultsVisualisation_CohortsDemographics_ui("test")
  ),
  function(input,output,session){
    mod_resultsVisualisation_CohortsDemographics_server("test",analysisResults)
  },
  options = list(launch.browser=TRUE)
)

app
