mod_fct_phenotypeFlags_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shinyjs::useShinyjs(),
        shiny::actionButton(ns("addFlag_button"), "Add Flag"),
        shinyjqui::sortableTableOutput(ns("flagTable_sortableTable"))
    )
}

mod_fct_phenotypeFlags_server <- function(id, r_groupOfCovariatesObject) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r <- shiny::reactiveValues(
            r_groupItems = NULL,
            flagBuildMessage = NULL,
            flagToBeAdded = NULL,
            flagTable = tibble::tibble(
                flagName = character(),
                flagColor = character(),
                flagFormula = character(),
                formula = character()
            )
        )

        #
        # update the group items
        #
        shiny::observe({
            r$r_groupItems <- setNames(
                r_groupOfCovariatesObject$groupsTibble$groupId,
                r_groupOfCovariatesObject$groupsTibble$groupName
            )
        })
        #
        # When add flag button is clicked, show modal dialog
        #
        shiny::observeEvent(input$addFlag_button, {
            shiny::req(r_groupOfCovariatesObject$groupsTibble |> nrow() > 0)
            shiny::req(r_groupOfCovariatesObject$personGroupsTibble)

            shiny::showModal(shiny::modalDialog(
                shiny::tags$h4("Add Flag"),
                shiny::textInput(ns("flagName_textinput"), "Flag Name", width = "100%", value = "", placeholder = "Enter flag name"),
                colourpicker::colourInput(ns("flagColor_colorinput"), "Flag Color", value = "red", palette = "limited"),
                mod_fct_dragAndDropTotalFormula_ui(ns("flagFormula_formula")),
                shiny::tags$h4("Flag message:"),
                shiny::verbatimTextOutput(ns("flagToBeAdded_textoutput"), placeholder = TRUE),
                shiny::actionButton(ns("acceptFlag_actionButton"), "Accept"),
                shiny::modalButton("Cancel"),
                footer = NULL
            ))
        })

        #
        # render the flag formula builder
        #
        rf_operationExpression <- mod_fct_dragAndDropTotalFormula_server(
            id = "flagFormula_formula",
            r_groupItems = r$r_groupItems,
            operatorItems = c(
                `(` = "(", `)` = ")",
                `<` = "<",
                `>` = ">",
                `>=` = ">=",
                `<=` = "<=",
                `==` = "==",
                `!=` = "!=",
                `+` = "+",
                `-` = "-",
                `*` = "*",
                `/` = "/"
            ),
            placeholder = "Drag and Drop COHORTS and OPERATORS here to create an expression"
        )

        #
        # validate the formula
        #
        shiny::observe({
            shiny::req(r_groupOfCovariatesObject$personGroupsTibble |> nrow() > 0)
            shiny::req(rf_operationExpression())

            operationExpression <- rf_operationExpression()
            expressionIds <- operationExpression$ids
            expressionNames <- operationExpression$names

            errorMessage <- NULL
            tryCatch(
                {
                    numberOfPersonsInFlag <- eval(parse(text = paste(
                        "r_groupOfCovariatesObject$personGroupsTibble |>",
                        "dplyr::filter(", expressionIds, ") |>",
                        "nrow()"
                    )))
                },
                error = function(e) {
                    errorMessage <<- e$message
                }
            )

            if (!is.null(errorMessage)) {
                r$flagBuildMessage <- errorMessage
                r$flagToBeAdded <- NULL
            } else {
                r$flagBuildMessage <- paste("Number of persons in flag:", numberOfPersonsInFlag)
                r$flagToBeAdded <- list(
                    flagName = input$flagName_textinput,
                    flagColor = input$flagColor_colorinput,
                    flagFormula = expressionNames,
                    formula = expressionIds
                )
            }
    
        })

        #
        # show message if flag is valid
        #
        output$flagToBeAdded_textoutput <- shiny::renderText({
            shiny::req(r$flagBuildMessage)
            r$flagBuildMessage
        })

        #
        # accept flag
        #
        shiny::observeEvent(input$acceptFlag_actionButton, {
            shiny::req(r$flagToBeAdded)
            shiny::removeModal()

            r$flagTable <- rbind(r$flagTable, r$flagToBeAdded)
            r$flagToBeAdded <- NULL
            r$flagBuildMessage <- NULL
        })

        #
        # render the flag table
        #
        output$flagTable_sortableTable <- shiny::renderTable(
            r$flagTable,
            rowNames = TRUE
        )


        #
        # Flag table to flag formulas
        #
        rf_flagFormulas <- shiny::reactive({
            shiny::req(r$flagTable)

            flagTable <- r$flagTable
            flagTable  <- flagTable |> dplyr::mutate(formula = paste(formula, ' ~ ', flagName))

            return(flagTable$formula)
        })

        #
        # return the flag formulas
        #
        return(rf_flagFormulas)
    })
}
