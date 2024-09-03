

devtools::load_all(".")

pathToCO2AnalysisModulesConfigYalm  <- testthat::test_path("config", "atlasDemo_CO2AnalysisModulesConfig.yml")

run_app(
  pathToCO2AnalysisModulesConfigYalm = pathToCO2AnalysisModulesConfigYalm,
  options = list(
    port = 5907,
    launch.browser=TRUE
    )
  )
