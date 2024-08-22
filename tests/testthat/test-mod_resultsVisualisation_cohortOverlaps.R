#
# test_that("mod_resultsVisualisation_CohortsOverlaps works", {
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
#   analysisResults <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)
#   on.exit({duckdb::dbDisconnect(analysisResults)})
#
#   # run module
#   shiny::testServer(
#     mod_resultsVisualisation_CohortsOverlaps_server,
#     args = list(
#       id = "test",
#       analysisResults = analysisResults
#     ),
#     {
#       output$upset_plot |> class() |> expect_equal("reactive")
#     }
#   )
#
# })




