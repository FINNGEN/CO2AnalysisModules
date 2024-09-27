#
# SELECT DATABASE and CO2 CONFIGURATION
#
testingDatabase <- "EunomiaGiBleed"
testingCO2AnalysisModulesConfig <- "AtlasDemo"

# check correct settings
possibleDatabases <- c("EunomiaGiBleed", "EunomiaMIMIC", "EunomiaFinnGen", "AtlasDevelopment")
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
if (testingDatabase %in% c("EunomiaGiBleed", "EunomiaMIMIC") ) {

  if( Sys.getenv("EUNOMIA_DATA_FOLDER") == "" ){
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  if(testingDatabase == "EunomiaGiBleed"){
    eunomiaDataSetName <- "GiBleed"
  }

  if(testingDatabase == "EunomiaMIMIC"){
    eunomiaDataSetName <- "MIMIC"
  }

  eunomiaDatabaseFile  <- Eunomia::getDatabaseFile(eunomiaDataSetName, overwrite = FALSE)

  test_databaseConfig <- readAndParseYalm(
    pathToYalmFile = testthat::test_path("config", "eunomia_cohortTableHandlerConfig.yml"),
    eunomiaDataSetName = eunomiaDataSetName,
    pathToEunomiaSqlite = eunomiaDatabaseFile
  )

  test_cohortTableHandlerConfig  <- test_databaseConfig$cohortTableHandler
}

#
# FinnGen Eunomia database
#
if (testingDatabase %in% c("EunomiaFinnGen") ) {

  if( Sys.getenv("EUNOMIA_DATA_FOLDER") == "" ){
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  urlToFinnGenEunomiaZip <- "https://raw.githubusercontent.com/FINNGEN/EunomiaDatasets/main/datasets/FinnGenR12/FinnGenR12_v5.4.zip"
  eunomiaDataFolder <- Sys.getenv("EUNOMIA_DATA_FOLDER")

  # Download the database if it doesn't exist
  if (!file.exists(file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite"))){

    result <- utils::download.file(
      url = urlToFinnGenEunomiaZip,
      destfile = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip"),
      mode = "wb"
    )

    Eunomia::extractLoadData(
      from = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip"),
      to = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite"),
      cdmVersion = '5.4',
      verbose = TRUE
    )
  }

  # copy to a temp folder
  file.copy(
    from = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite"),
    to = file.path(tempdir(), "FinnGenR12_v5.4.sqlite"),
    overwrite = TRUE
  )

  test_databaseConfig <- readAndParseYalm(
    pathToYalmFile = testthat::test_path("config", "eunomia_cohortTableHandlerConfig.yml"),
    eunomiaDataSetName =  "FinnGenR12",
    pathToEunomiaSqlite = file.path(tempdir(), "FinnGenR12_v5.4.sqlite")
  )

  test_cohortTableHandlerConfig  <- test_databaseConfig$cohortTableHandler
}

#
# AtlasDevelopmet Database
#
if (testingDatabase %in% c("AtlasDevelopment") ) {

  if( Sys.getenv("GCP_SERVICE_KEY") == "" ){
    message("GCP_SERVICE_KEY not set. Please set this environment variable to the path of the GCP service key.")
    stop()
  }

  if( Sys.getenv("DATABASECONNECTOR_JAR_FOLDER") == "" ){
    message("DATABASECONNECTOR_JAR_FOLDER not set. Please set this environment variable to the path of the database connector jar folder.")
    stop()
  }


  test_databaseConfig <- readAndParseYalm(
    pathToYalmFile = testthat::test_path("config", "atlasDevelopment_cohortTableHandlerConfig.yml"),
    OAuthPvtKeyPath = Sys.getenv("GCP_SERVICE_KEY"),
    pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
  )

  test_test_cohortTableHandlerConfig  <- test_databaseConfig$cohortTableHandler
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

