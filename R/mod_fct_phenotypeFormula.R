

mod_fct_dragAndDropFormula_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::uiOutput(ns("operation_expression"))
  )
}

mod_fct_dragAndDropFormula_server <- function(id, r_groupedCovariates, operatorItems, placeholder) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # Update is not working, we build the ui in server
    #
    output$operation_expression <- shiny::renderUI({
      shiny::req(r_groupedCovariates$groupedCovariatesTibble)

      groupItems <- setNames(
        r_groupedCovariates$groupedCovariatesTibble$groupId,
        r_groupedCovariates$groupedCovariatesTibble$groupName
      )
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
    # calculates formula
    #
    rf_formula <- shiny::reactive({
      shiny::req(r_groupedCovariates$groupedCovariatesTibble)
      shiny::req(input$dest_boxes)
      shiny::req(input$dest_boxes != placeholder)

      groupItems <- setNames(
        r_groupedCovariates$groupedCovariatesTibble$groupId,
        r_groupedCovariates$groupedCovariatesTibble$groupName
      )

      expresionElements <- input$dest_boxes

      expressionNames <- sapply(expresionElements, function(x) {
        if (grepl("^g\\d+$", x)) {
          names(groupItems)[groupItems == x]
        } else {
          x
        }
      })

      formula <- list(
        formula = paste(expresionElements, collapse = ""),
        formulaPretty = paste(expressionNames, collapse = "")
      )

      return(formula)
     
    })


    #
    # returns the formula
    #
    return(rf_formula)
  })
}
