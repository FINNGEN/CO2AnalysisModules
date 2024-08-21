# settings

testConfigFile <- "eunomia_cohortTableHandlerConfig.yml"
cohortTableHandlerConfig <- yaml::read_yaml(testthat::test_path("config", testConfigFile))$cohortTableHandler

# if using eunomia database create the file
if(cohortTableHandlerConfig$connection$connectionDetailsSettings$server == "eunomia_default"){
  eunomiaConnectionDetails  <- Eunomia::getEunomiaConnectionDetails()
  cohortTableHandlerConfig$connection$connectionDetailsSettings$server <- eunomiaConnectionDetails$server()
}

# inform user
message("************* Testing on ", testConfigFile, " *************")

