
helper_createNewCohortTableHandler <- function(addCohorts = NULL){

  addCohorts |> checkmate::assertCharacter(len = 1, null.ok = TRUE)
  addCohorts |> checkmate::assertSubset(c(
    "EunomiaDefaultCohorts", "HadesExtrasFractureCohorts","HadesExtrasAsthmaCohorts",
    "HadesExtrasFractureCohortsMatched","HadesExtrasAsthmaCohortsMatched"
    ), empty.ok = TRUE)

  cohortTableHandlerConfig <- test_cohortTableHandlerConfig # set by setup.R
  loadConnectionChecksLevel = "basicChecks"

  # TEMP
  # name with current time with milisecond to avoid conflicts
  cohortTableHandlerConfig$cohortTable$cohortTableName <- paste0(
    cohortTableHandlerConfig$cohortTable$cohortTableName, "_",
    gsub('\\.','',format(Sys.time(), "d%H%M%S%OS3"))
  )
  # END TEMP
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
    if(addCohorts == "HadesExtrasAsthmaCohorts"){
      cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
        settingsFileName = "testdata/asthma/Cohorts.csv",
        jsonFolder = "testdata/asthma/cohorts",
        sqlFolder = "testdata/asthma/sql/sql_server",
        cohortFileNameFormat = "%s",
        cohortFileNameValue = c("cohortId"),
        subsetJsonFolder = "testdata/asthma/cohort_subset_definitions/",
        packageName = "HadesExtras",
        verbose = FALSE
      )
    }
    if(addCohorts == "HadesExtrasFractureCohortsMatched"){
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

      # Match
      subsetDef <- CohortGenerator::createCohortSubsetDefinition(
        name = "",
        definitionId = 1,
        subsetOperators = list(
          HadesExtras::createMatchingSubset(
            matchToCohortId = 1,
            matchRatio = 10,
            matchSex = TRUE,
            matchBirthYear = TRUE,
            matchCohortStartDateWithInDuration = FALSE,
            newCohortStartDate = "asMatch",
            newCohortEndDate = "keep"
          )
        )
      )

      cohortDefinitionSet <- cohortDefinitionSet |>
        CohortGenerator::addCohortSubsetDefinition(subsetDef, targetCohortIds = 2)
    }
    if(addCohorts == "HadesExtrasAsthmaCohortsMatched"){
      # cohorts from eunomia
      cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
        settingsFileName = "testdata/asthma/Cohorts.csv",
        jsonFolder = "testdata/asthma/cohorts",
        sqlFolder = "testdata/asthma/sql/sql_server",
        cohortFileNameFormat = "%s",
        cohortFileNameValue = c("cohortId"),
        subsetJsonFolder = "testdata/asthma/cohort_subset_definitions/",
        packageName = "HadesExtras",
        verbose = FALSE
      )

      # Match to sex and bday, match ratio 10
      subsetDef <- CohortGenerator::createCohortSubsetDefinition(
        name = "",
        definitionId = 1,
        subsetOperators = list(
          HadesExtras::createMatchingSubset(
            matchToCohortId = 1,
            matchRatio = 10,
            matchSex = TRUE,
            matchBirthYear = TRUE,
            matchCohortStartDateWithInDuration = FALSE,
            newCohortStartDate = "asMatch",
            newCohortEndDate = "keep"
          )
        )
      )

      cohortDefinitionSet <- cohortDefinitionSet |>
        CohortGenerator::addCohortSubsetDefinition(subsetDef, targetCohortIds = 2)

    }
    cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)
  }

  return(cohortTableHandler)
}


