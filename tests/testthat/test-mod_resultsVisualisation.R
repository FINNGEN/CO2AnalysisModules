
test_that("mod_resultsVisualisation works", {

  # set up
  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({rm(cohortTableHandler);gc()})

  exportFolder <- withr::local_tempdir('testCohortOverlaps')

  analysisSettings <- list(
    cohortIds = c(1, 2),
    minCellCount = 1
  )

  pathToResultsDatabase <- execute_CohortOverlaps(
    exportFolder = exportFolder,
    cohortTableHandler = cohortTableHandler,
    analysisSettings = analysisSettings
  )

  analysisResults <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)
  withr::defer({duckdb::dbDisconnect(analysisResults)})

  mod_resultsVisualisation_dummy_server <- function(id,analysisResults){
    shiny::moduleServer(id, function(input, output, session){})
  }

  # run module
  shiny::testServer(
    mod_resultsVisualisation_server, # The module to test
    args = list(
      id = "test",
      resultsVisualisationModuleServer = mod_resultsVisualisation_dummy_server,
      analysisResults = analysisResults
    ),    # No additional arguments needed
    {
      # Check that the rendered text output is as expected
      output$cohortsInfo  |> class() |> expect_equal("json")
    }
  )

})
