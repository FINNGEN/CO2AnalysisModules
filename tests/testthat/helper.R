
helper_createNewCohortTableHandler <- function(addCohorts = NULL){

  addCohorts |> checkmate::assertCharacter(len = 1, null.ok = TRUE)
  addCohorts |> checkmate::assertSubset(c("EunomiaDefaultCohorts", "HadesExtrasFractureCohorts"), empty.ok = TRUE)

  cohortTableHandlerConfig <- cohortTableHandlerConfig # set by setup.R
  loadConnectionChecksLevel = "basicChecks"

  cohortTableHandler <- HadesExtras::createCohortTableHandlerFromList(cohortTableHandlerConfig, loadConnectionChecksLevel)

  if(!is.null(addCohorts) ){
    if(addCohorts == "EunomiaDefaultCohorts"){
      cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
        settingsFileName = "testdata/name/Cohorts.csv",
        jsonFolder = "testdata/name/cohorts",
        sqlFolder = "testdata/name/sql/sql_server",
        cohortFileNameFormat = "%s",
        cohortFileNameValue = c("cohortName"),
        packageName = "CohortGenerator",
        verbose = FALSE
      )
    }
    if(addCohorts == "HadesExtrasFractureCohorts"){
      cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
        settingsFileName = "testdata/fracture/Cohorts.csv",
        jsonFolder = "testdata/fracture/cohorts",
        sqlFolder = "testdata/fracture/sql/sql_server",
        cohortFileNameFormat = "%s",
        cohortFileNameValue = c("cohortId"),
        subsetJsonFolder = "testdata/fracture/cohort_subset_definitions/",
        packageName = "HadesExtras",
        verbose = T
      )
    }
    cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)
  }

  return(cohortTableHandler)
}
