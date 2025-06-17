

mod_fct_dragAndDropTotalFormula_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::uiOutput(ns("operation_expression"))
  )
}

mod_fct_dragAndDropTotalFormula_server <- function(id, r_groupItems, operatorItems, placeholder) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # Update is not working, we build the ui in server
    #
    output$operation_expression <- shiny::renderUI({
      shiny::req(r_groupItems)

      groupItems <- r_groupItems
      operatorItems <- operatorItems
      numbersItems <- 0:9

      htmltools::tagList(
        shinydashboard::box(
          title = "Expression defining the flag",
          width = 12,
          background = "light-blue",
          shinyjqui::orderInput(
            inputId = ns("dest_boxes"),
            width = "100%",
            label = NULL,
            items = NULL,
            placeholder = placeholder
          )
        ),
        shinydashboard::box(
          width = 12,
          shinyjqui::orderInput(
            inputId = ns("source_boxes_cohorts"),
            width = "100%",
            label = "COHORTS",
            items = groupItems,
            as_source = TRUE, connect = ns("dest_boxes")
          ),
          shinyjqui::orderInput(
            inputId = ns("source_boxes"),
            width = "100%",
            label = "OPERATORS",
            items = operatorItems,
            as_source = TRUE, connect = ns("dest_boxes")
          ),
          shinyjqui::orderInput(
            inputId = ns("source_boxes"),
            width = "100%",
            label = "NUMBERS",
            items = numbersItems,
            as_source = TRUE, connect = ns("dest_boxes")
          )
        )

      )
    })

    #
    # calculates cohort operation expression
    #
    rf_operationExpression <- shiny::reactive({
      shiny::req(r_groupItems)
      shiny::req(input$dest_boxes)
      shiny::req(input$dest_boxes != placeholder)

      groupItems <- r_groupItems

      expresionElements <- input$dest_boxes

      expressionNames <- sapply(expresionElements, function(x) {
        if (grepl("^g\\d+$", x)) {
          names(groupItems)[groupItems == x]
        } else {
          x
        }
      })

      operationExpression <- list(
        ids = paste(expresionElements, collapse = " "),
        names = paste(expressionNames, collapse = " ")
      )

      return(operationExpression)
     
    })


    #
    # returns the operation expresion
    #
    return(rf_operationExpression)
  })
}
