

# run module --------------------------------------------------------------
devtools::load_all(".")

shiny::shinyApp(
  shiny::fluidPage(
    mod_fct_formTimeWindows_ui("test")
  ),
  function(input,output,session){
    output <- mod_fct_formTimeWindows_server("test", session)

    observe({
        print(output())
    })

  },
  options = list(launch.browser=TRUE)
)
