# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

# run module --------------------------------------------------------------
devtools::load_all(".")

groupsTibble <- tibble::tibble(
          groupId = c("g1", "g2", "g3"),
          groupName = c("Group 1", "Group 2", "Group 3"),
          covariateIds = list("1", "2", "3"),
          conceptCodes = list("1", "2", "3"),
          covariateNames = list("1", "2", "3"),
          covariatesDistribution = list("1", "2", "3")
        )

personGroupsTibble <- tibble::tibble(
  'g1' = c(1, 0, 0),
  'g2' = c(0, 1, 0),
  'g3' = c(0, 0, 1)
)

shiny::shinyApp(
  shiny::fluidPage(
    mod_fct_phenotypeFlags_ui("test")
  ),
  function(input, output, session) {
    r <- shiny::reactiveValues(
      groupOfCovariatesObject = list(
        groupsTibble = groupsTibble,
        personGroupsTibble = personGroupsTibble
      )
    )

    rf_flagFormulas <- mod_fct_phenotypeFlags_server("test", r$groupOfCovariatesObject)

    shiny::observe({
      print(rf_flagFormulas())
    })
  },
  options = list(launch.browser = TRUE)
)


