# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))


# RUN APP --------------------------------------------------------------
run_app(
  CO2AnalysisModulesConfig = test_CO2AnalysisModulesConfig,
  options = list(
    port = 8561,
    launch.browser=TRUE
    )
  )
