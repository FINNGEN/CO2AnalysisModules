#
# test_that("mod_resultsVisualisation_CohortsOverlaps works", {
#
#   # set up
#   cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
#   withr::defer({rm(cohortTableHandler);gc()})
#
#   exportFolder <- withr::local_tempdir('testCohortDemographics')
#
#   analysisSettings <- list(
#     cohortIds = c(1, 2),
#     referenceYears = c("cohort_start_date", "cohort_end_date", "birth_datetime"),
#     groupBy = c("calendarYear", "ageGroup", "gender"),
#     minCellCount = 1
#   )
#
#   pathToResultsDatabase <- execute_CohortDemographics(
#     exportFolder = exportFolder,
#     cohortTableHandler = cohortTableHandler,
#     analysisSettings = analysisSettings
#   )
#
#   analysisResults <- duckdb::dbConnect(duckdb::duckdb(), pathToResultsDatabase)
#   withr::defer({duckdb::dbDisconnect(analysisResults)})
#
#   # run module
#   shiny::testServer(
#     mod_resultsVisualisation_CohortsDemographics_server,
#     args = list(
#       id = "test",
#       analysisResults = analysisResults
#     ),
#     {
#
#       #set inputs
#       session$setInputs(
#         CDPlot_ui = NULL
#       )
#             browser()
#       output$demographicsPlot |> class() |> expect_equal("reactive")
#     }
#   )
#
# })
#
#
#
#
