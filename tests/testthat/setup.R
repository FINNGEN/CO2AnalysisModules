# settings
if( Sys.getenv("EUNOMIA_DATA_FOLDER") == "" ){
  message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
  stop()
}

testConfigFile <- "eunomia_cohortTableHandlerConfig.yml"
cohortTableHandlerConfig <- yaml::read_yaml(testthat::test_path("config", testConfigFile))$cohortTableHandler

# if using eunomia database create the file
if(cohortTableHandlerConfig$connection$connectionDetailsSettings$server == "Eunomia"){
  databaseName <- cohortTableHandlerConfig$connection$connectionDetailsSettings$databaseName
  eunomiaDatabaseFile  <- Eunomia::getDatabaseFile(databaseName, overwrite = FALSE)
  cohortTableHandlerConfig$connection$connectionDetailsSettings$server <- eunomiaDatabaseFile
  cohortTableHandlerConfig$connection$connectionDetailsSettings$databaseName <- NULL
}

# inform user
message("************* Testing on ", testConfigFile, " *************")

