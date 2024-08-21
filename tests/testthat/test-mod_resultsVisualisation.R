#
# test_that("mod_resultsVisualisation works", {
#
#   # set up
#   cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
#   on.exit({rm(cohortTableHandler);gc()})
#
#   exportFolder <- file.path(tempdir(), "testCohortOverlaps")
#   dir.create(exportFolder, showWarnings = FALSE)
#   on.exit({unlink(exportFolder, recursive = TRUE)})
#
#   analysisSettings <- list(
#     cohortIds = c(1, 2),
#     minCellCount = 1
#   )
#
#   pathToResultsDatabase <- execute_CohortOverlaps(
#     exportFolder = exportFolder,
#     cohortTableHandler = cohortTableHandler,
#     analysisSettings = analysisSettings
#   )
#
#   analysisResults <- pool::dbPool(drv = duckdb::duckdb(), dbdir=pathToResultsDatabase)
#   on.exit({pool::poolClose(analysisResults)})
#
#   mod_resultsVisualisation_dummy_server <- function(id,analysisResults){
#     shiny::moduleServer(id, function(input, output, session){})
#   }
#
#   # run module
#   shiny::testServer(
#     mod_resultsVisualisation_server,
#     args = list(
#       id = "test",
#       resultsVisualisationModuleServer = mod_resultsVisualisation_dummy_server,
#       analysisResults = analysisResults
#     ),
#     {
#
#       output$cohortDefinitions |> isreac
#     }
#   )
#
# })
