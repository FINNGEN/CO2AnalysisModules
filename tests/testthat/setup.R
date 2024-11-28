#
# SELECT DATABASE and CO2 CONFIGURATION
#

# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "Eunomia-GiBleed")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "AtlasDevelopment-DBI")
testingDatabase <- Sys.getenv("HADESEXTAS_TESTING_ENVIRONMENT")

testingCO2AnalysisModulesConfig <- "AtlasDemo"

# check correct settings
possibleDatabases <- c("Eunomia-GiBleed", "Eunomia-MIMIC", "Eunomia-FinnGen", "AtlasDevelopment", "AtlasDevelopment-DBI")
if( !(testingDatabase %in% possibleDatabases) ){
  message("Please select a valid database from: ", paste(possibleDatabases, collapse = ", "))
  stop()
}

possibleCO2AnalysisModulesConfig <- c("AtlasDemo", "PrivateAtlas")
if( !(testingCO2AnalysisModulesConfig %in% possibleCO2AnalysisModulesConfig) ){
  message("Please select a valid CO2 analysis modules configuration from: ", paste(possibleCO2AnalysisModulesConfig, collapse = ", "))
  stop()
}

#
# Eunomia Databases
#
if (testingDatabase |> stringr::str_starts("Eunomia")) {
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  pathToGiBleedEunomiaSqlite <- Eunomia::getDatabaseFile("GiBleed", overwrite = FALSE)
  pathToMIMICEunomiaSqlite <- Eunomia::getDatabaseFile("MIMIC", overwrite = FALSE)
  pathToFinnGenEunomiaSqlite <- helper_FinnGen_getDatabaseFile()


  test_databasesConfig <- HadesExtras::readAndParseYaml(
    pathToYalmFile = testthat::test_path("config", "eunomia_databasesConfig.yml"),
    pathToGiBleedEunomiaSqlite = pathToGiBleedEunomiaSqlite,
    pathToMIMICEunomiaSqlite = pathToMIMICEunomiaSqlite,
    pathToFinnGenEunomiaSqlite = pathToFinnGenEunomiaSqlite
  )

  if (testingDatabase |> stringr::str_ends("GiBleed")) {
    test_cohortTableHandlerConfig <- test_databasesConfig[[1]]$cohortTableHandle
  }
  if (testingDatabase |> stringr::str_ends("MIMIC")) {
    test_cohortTableHandlerConfig <- test_databasesConfig[[2]]$cohortTableHandle
  }
  if (testingDatabase |> stringr::str_ends("FinnGen")) {
    test_cohortTableHandlerConfig <- test_databasesConfig[[4]]$cohortTableHandle
  }

  # add test cohorts and cohort definitions
  helper_addCohortAndCohortDefinitionTables(cohortTableHandlerConfig = test_cohortTableHandlerConfig)
}


#
# AtlasDevelopmet Database
#
if (testingDatabase %in% c("AtlasDevelopment")) {
  if (Sys.getenv("GCP_SERVICE_KEY") == "") {
    message("GCP_SERVICE_KEY not set. Please set this environment variable to the path of the GCP service key.")
    stop()
  }

  if (Sys.getenv("DATABASECONNECTOR_JAR_FOLDER") == "") {
    message("DATABASECONNECTOR_JAR_FOLDER not set. Please set this environment variable to the path of the database connector jar folder.")
    stop()
  }

  test_databasesConfig <- HadesExtras::readAndParseYaml(
    pathToYalmFile = testthat::test_path("config", "atlasDev_databasesConfig.yml"),
    OAuthPvtKeyPath = Sys.getenv("GCP_SERVICE_KEY"),
    pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
  )

  test_cohortTableHandlerConfig <- test_databasesConfig[[1]]$cohortTableHandler
}

#
# AtlasDevelopmet-DBI Database
#
if (testingDatabase %in% c("AtlasDevelopment-DBI")) {
  if (Sys.getenv("GCP_SERVICE_KEY") == "") {
    message("GCP_SERVICE_KEY not set. Please set this environment variable to the path of the GCP service key.")
    stop()
  }

  bigrquery::bq_auth(path = Sys.getenv("GCP_SERVICE_KEY"))

  test_databasesConfig <- HadesExtras::readAndParseYaml(
    pathToYalmFile = testthat::test_path("config", "atlasDev_DBI_databasesConfig.yml")
  )

  test_cohortTableHandlerConfig <- test_databasesConfig[[1]]$cohortTableHandler
}

#
# CO2 Analysis Modules Configuration
#
if(testingCO2AnalysisModulesConfig == "AtlasDemo"){
  pathToCO2AnalysisModulesConfigYalm  <-  testthat::test_path("config", "atlasDemo_CO2AnalysisModulesConfig.yml")
  test_CO2AnalysisModulesConfig <- readAndParseYalm(pathToCO2AnalysisModulesConfigYalm)
}
if(testingCO2AnalysisModulesConfig == "PrivateAtlas"){
  pathToCO2AnalysisModulesConfigYalm  <-  testthat::test_path("config", "privateAtlas_CO2AnalysisModulesConfig.yml")
  test_CO2AnalysisModulesConfig <- readAndParseYalm(pathToCO2AnalysisModulesConfigYalm)
}

#
# INFORM USER
#
message("************* Testing on: ")
message("Database: ", testingDatabase)
message("CO2 Analysis Modules Configuration: ", testingCO2AnalysisModulesConfig)

