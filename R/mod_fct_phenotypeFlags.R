mod_fct_phenotypeFlags_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shinyjs::useShinyjs(),
        shiny::actionButton(ns("addFlag_button"), "Add Flag"),
        shinyjqui::sortableTableOutput(ns("flagTable_sortableTable"))
    )
}

mod_fct_phenotypeFlags_server <- function(id, r_groupedCovariates) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r <- shiny::reactiveValues(
            flagBuildMessage = NULL,
            flagToBeAdded = NULL,
            flagsTable = tibble::tibble(
                flagName = character(),
                flagColor = character(),
                flagRulePretty = character(),
                flagRule = character()
            )
        )

        #
        # When add flag button is clicked, show modal dialog
        #
        shiny::observeEvent(input$addFlag_button, {
            shiny::req(r_groupedCovariates$groupedCovariatesTibble |> nrow() > 0)
            shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble)

            shiny::showModal(shiny::modalDialog(
                shiny::tags$h4("Add Flag"),
                shiny::textInput(ns("flagName_textinput"), "Flag Name", width = "100%", value = "", placeholder = "Enter flag name"),
                colourpicker::colourInput(ns("flagColor_colorinput"), "Flag Color", value = "red", palette = "limited"),
                mod_fct_dragAndDropFormula_ui(ns("flagFormula_formula")),
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
        rf_formula <- mod_fct_dragAndDropFormula_server(
            id = "flagFormula_formula",
            r_groupedCovariates = r_groupedCovariates,
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
                `/` = "/",
                `&` = "&",
                `|` = "|",
                `!` = "!"
            ),
            placeholder = "Drag and Drop to create rule"
        )

        #
        # validate the formula
        #
        shiny::observe({
            shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble |> nrow() > 0)
            shiny::req(rf_formula())

            flagRuleFormula <- rf_formula()
            flagRule <- flagRuleFormula$formula

            errorMessage <- NULL
            tryCatch(
                {
                    numberOfPersonsInFlag <- eval(parse(text = paste(
                        "r_groupedCovariates$groupedCovariatesPerPersonTibble |>",
                        "dplyr::filter(", flagRule, ") |>",
                        "nrow()"
                    )))
                },
                error = function(e) {
                    errorMessage <<- e$message
                }
            )

            if (!is.null(errorMessage)) {
                r$flagBuildMessage <- paste("Error:", errorMessage)
                r$flagToBeAdded <- NULL
            } else {
                r$flagBuildMessage <- paste(
                    "Flag rule:", flagRuleFormula$formulaPretty, "\n",
                    "Number of persons in flag:", numberOfPersonsInFlag
                )
                r$flagToBeAdded <- list(
                    flagName = input$flagName_textinput,
                    flagColor = input$flagColor_colorinput,
                    flagRulePretty = flagRuleFormula$formulaPretty,
                    flagRule = flagRuleFormula$formula
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
        # accept flag, copy form to flagTable
        #
        shiny::observeEvent(input$acceptFlag_actionButton, {
            shiny::req(r$flagToBeAdded)
            shiny::removeModal()

            r$flagsTable <- rbind(r$flagsTable, r$flagToBeAdded)
            r$flagToBeAdded <- NULL
            r$flagBuildMessage <- NULL
        })

        #
        # render the flag table
        #
        output$flagTable_sortableTable <- shiny::renderTable(
            r$flagsTable,
            rowNames = TRUE
        )


        #
        # Return the flag table
        #
        rf_flagsTable <- shiny::reactive({
            shiny::req(r$flagsTable)
            return(r$flagsTable)
        })

        return(rf_flagsTable)
    })
}
