# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

# run module --------------------------------------------------------------
devtools::load_all(".")

shiny::shinyApp(
  shiny::fluidPage(
    mod_fct_dragAndDropTotalFormula_ui("test")
  ),
  function(input, output, session) {
    r_groupItems <- shiny::reactiveValues(
      groupItems = setNames(c("g1", "g2", "g3"), c("Group 1", "Group 2", "Group 3"))
    )
    operatorItems <- c(`(`="(", `)`=")",
                      `<`="<",
                      `>`=">",
                      `>=`=">=",
                      `<=`="<=",
                      `==`="==",
                      `!=`="!=",
                      `+`="+",
                      `-`="-",
                      `*`="*",
                      `/`="/")
    placeholder <- "Drag and Drop COHORTS and OPERATORS here to create an expression"

    rf_operationExpression <- mod_fct_dragAndDropTotalFormula_server("test", r_groupItems$groupItems, operatorItems, placeholder)

    shiny::observe({
      print(rf_operationExpression()$ids)
      print(rf_operationExpression()$names)
    })
  },
  options = list(launch.browser = TRUE)
)


app