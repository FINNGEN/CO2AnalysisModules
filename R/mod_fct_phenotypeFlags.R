mod_fct_phenotypeFlags_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shinyjs::useShinyjs(),
        shiny::actionButton(ns("addFlag_button"), "Add Flag"),
        #reactable::reactableOutput(ns("flagTable_sortableTable")),
        shinyjqui::jqui_sortable(shinyjqui::sortableTableOutput(ns("flagTable_sortableTable"))),

        # Custom CSS for grab/grabbing cursor on the sortable rows
        shiny::tags$style(HTML(sprintf("
         #%s table tr {
          cursor: grab;
          transition: background-color 0.2s ease;
          }
          #%s table tr:active {
            cursor: grabbing;
            border: 2px solid #ffa500;
            background-color: #eaffaa;
            box-shadow: 0 0 5px rgba(0,0,0,0.2);
          }
          #%s table tr:hover {
            background-color: #f0fff0;
          }
          #%s table tr i.fa-grip-vertical {
            cursor: grab;
            color: #888;
          }

          #%s table th:nth-child(8) {
            display: none;  /* hide the drag column headers */
          }
        ", ns("flagTable_sortableTable"), ns("flagTable_sortableTable"), ns("flagTable_sortableTable"),
          ns("flagTable_sortableTable"),ns("flagTable_sortableTable")))),

        # Custom CSS to remove grey background and border from verbatimTextOutput
        shiny::tags$style(HTML(sprintf("
          #%s {
            background-color: transparent !important;
            border: none !important;
            padding: 0 !important;
            font-family: monospace;
            white-space: pre-wrap;
          }
        ", ns("flagToBeAdded_textoutput"))))

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
                flagRule = character(),
                nPersonsInFlag = numeric()
            ),
            flagBeingEditedIndex = NULL,
            editingColorIndex = NULL,
            lastFlagsTableRowOrder = NULL
        )

        flagTable_row_order <- reactive({

          order_df <- input$flagTable_sortableTable_order

          # shinyjqui::sortableTableOutput did not work as it should normally, it was not tracking the row orders.
          # So wrapped with jqui_sortable above, and order extracted from the text column of input$flagTable_sortableTable_order returned

          # If input not available or empty, return NULL or default order
          if (is.null(order_df) || nrow(order_df) == 0 || all(is.na(order_df$text))) {
            return(NULL)  # or return default: seq_len(nrow(flagsTable)) if you have flagsTable available
          }

          # Extract numeric row numbers from text column (before first tab)
          new_order <- as.numeric(sub("\t.*", "", order_df$text))

          if (any(is.na(new_order))) {
            return(NULL)
          }

          new_order
        })

        #
        # When add flag button is clicked, show modal dialog
        #
        shiny::observeEvent(input$addFlag_button, {
            shiny::req(r_groupedCovariates$groupedCovariatesTibble |> nrow() > 0)
            shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble)

            rf_formula_res$set_formula(formula_string=NULL)

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
        operators_flag = c(`(` = "(", `)` = ")",`<` = "<",`>` = ">",`>=` = ">=",
                           `<=` = "<=", `==` = "==",`!=` = "!=",`+` = "+",`-` = "-",
                           `*` = "*",`/` = "/",`&` = "&",`|` = "|",`!` = "!")
        rf_formula_res <- mod_fct_dragAndDropFormula_server(
            id = "flagFormula_formula",
            r_groupedCovariates = r_groupedCovariates,
            operatorItems = operators_flag,
            titleText = "Expression that evaluates to true or false:",
            placeholder = "Drag and Drop to create rule"
        )
        rf_formula = rf_formula_res$get_formula

        is_flagformula_incomplete <- function(formula, operators) {

          f <- trimws(formula)
          if (nchar(f) == 0) {
            return(TRUE)
          }

          escape_regex <- function(x) {
            gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
          }

          escaped_ops <- escape_regex(operators)
          invalid_start_ends <- escape_regex(c("<", ">", ">=", "<=", "==", "!=","!","+","=" ,"-", "*", "/", "&", "|"))

          ops_pattern_end <- paste0("(", paste0(invalid_start_ends, collapse = "|"), ")$")

          # formula should not end with these operators
          if (grepl(ops_pattern_end, f)) {
            return(TRUE)
          }

          # formula should not start with these operators
          ops_pattern_start <- paste0("^(", paste(invalid_start_ends, collapse = "|"), ")")
          if (grepl(ops_pattern_start, f)) return(TRUE)

          # check for unmatched parentheses
          open_parens <- stringr::str_count(f, stringr::fixed("("))
          close_parens <- stringr::str_count(f, stringr::fixed(")"))

          if (open_parens != close_parens) {
            return(TRUE)
          }

          # just variable with no operators
          contains_operator <- any(sapply(escaped_ops, function(op) grepl(op, f)))
          if (!contains_operator) return(TRUE)

          return(FALSE)

        }


        #
        # validate the formula
        #
        shiny::observe({
            shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble |> nrow() > 0)
            shiny::req(rf_formula())

            flagRuleFormula <- rf_formula()
            flagRule <- flagRuleFormula$formula

            # Check if formula is potentially incomplete (e.g., ends with operator or is empty)
            if (is_flagformula_incomplete(flagRule, operators_flag)) {
              r$flagBuildMessage <- NULL
              r$flagToBeAdded <- NULL
              return()
            }

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
                    errorMessage <<- paste("formula syntax: formula should result in logical values. Details:", e$message)
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
                    flagRule = flagRuleFormula$formula,
                    nPersonsInFlag = numberOfPersonsInFlag
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
        observeEvent(input$acceptFlag_actionButton, {
          shiny::req(r$flagToBeAdded)

          if (is.null(r$flagBeingEditedIndex)) {
            # Add new
            r$flagsTable <- rbind(r$flagsTable, r$flagToBeAdded)
          } else {
            # Edit existing
            r$flagsTable[r$flagBeingEditedIndex, ] <- r$flagToBeAdded
            r$flagBeingEditedIndex <- NULL
          }

          r$flagToBeAdded <- NULL
          r$flagBuildMessage <- NULL
          r$flagBeingEditedIndex <- NULL
          removeModal()
        })


        #
        # render the flag table
        #

        # # render the flags
        # output$flagTable_sortableTable <- reactable::renderReactable({
        #   shiny::req(r$flagsTable)
        #
        #   df <- r$flagsTable
        #   if (nrow(df) == 0) return(NULL)
        #
        #   flags <- r$flagsTable |>
        #     dplyr::mutate(editButton = NA, deleteButton = NA)
        #
        #   columns <- list(
        #     flagName = reactable::colDef(name = "Flag Name"),
        #     flagColor = reactable::colDef(
        #       name = "Color",
        #       cell = function(value, index) {
        #         inputId <- ns(paste0("flagColor_", index))
        #         htmltools::tags$div(
        #           style = sprintf("width:40px;height:20px;background:%s;cursor:pointer;border:1px solid #ccc;", value),
        #           onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("edit_color"), index)
        #         )
        #       },
        #       sortable = FALSE,
        #       maxWidth = 60
        #     ),
        #     flagRulePretty = reactable::colDef(name = "Rule"),
        #     editButton = reactable::colDef(
        #       name = "",
        #       sortable = FALSE,
        #       cell = function(value, index) {
        #         htmltools::tags$button(
        #           shiny::icon("pen"),
        #           class = "btn btn-outline-primary btn-sm",
        #           onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("edit_flag"), index)
        #         )
        #       },
        #       maxWidth = 50
        #     ),
        #     deleteButton = reactable::colDef(
        #       name = "",
        #       sortable = FALSE,
        #       cell = function(value, index) {
        #         htmltools::tags$button(
        #           shiny::icon("trash"),
        #           class = "btn btn-outline-danger btn-sm",
        #           onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("delete_flag"), index)
        #         )
        #       },
        #       maxWidth = 50
        #     )
        #   )
        #
        #   reactable::reactable(flags, columns = columns, resizable = TRUE)
        # })

        #
        # render the flag table
        #
        # render the flags
        output$flagTable_sortableTable <- shiny::renderTable({
          shiny::req(r$flagsTable)
          df <- r$flagsTable
          if (nrow(df) == 0) return(NULL)

          # Build the table manually with HTML
          df_display <- df

          df_display$flagColor <- sprintf(
            "<div style='width:40px;height:20px;background:%s;cursor:pointer;border:1px solid #ccc;'
             onclick=\"Shiny.setInputValue('%s', %d, {priority: 'event'})\"></div>",
            df$flagColor, ns("edit_color"), seq_len(nrow(df))
          )

          df_display$edit <- sprintf(
            "<button class='btn btn-outline-primary btn-sm'
            onclick=\"Shiny.setInputValue('%s', %d, {priority: 'event'})\">
            <i class='fa fa-pen'></i></button>",
            ns("edit_flag"), seq_len(nrow(df))
          )

          df_display$delete <- sprintf(
            "<button class='btn btn-outline-danger btn-sm'
            onclick=\"Shiny.setInputValue('%s', %d, {priority: 'event'})\">
            <i class='fa fa-trash'></i></button>",
            ns("delete_flag"), seq_len(nrow(df))
          )


          # Drag handle column
          df_display$orderFlags <- sprintf(
                "<div style='display:flex; justify-content:center; align-items:center; height:32px;'>
                 <i class='fa fa-grip-vertical'></i>
               </div>"
              )

          #column to display
          df_display <- df_display[, c("flagName", "flagColor", "flagRulePretty", "nPersonsInFlag", "edit", "delete","orderFlags")]

          df_display
        }, rownames = TRUE, sanitize.text.function = function(x) x,align = "l")


        # edit color from rows
        observeEvent(input$edit_color, {
          idx <- input$edit_color
          current_color <- r$flagsTable$flagColor[idx]

          showModal(modalDialog(
            title = "Pick a new color",
            colourpicker::colourInput(ns("new_color"), "Change Color", value = current_color,palette = "limited"),
            footer = tagList(
              actionButton(ns("save_color"), "Save"),
              modalButton("Cancel")
            )
          ))

          # Save the index in a reactiveVal to use later
          r$editingColorIndex <- idx
        })

        observeEvent(input$save_color, {
          idx <- r$editingColorIndex
          r$flagsTable$flagColor[idx] <- input$new_color

          removeModal()
        })


        # Deletion confirmation
        observeEvent(input$delete_flag, {
          index <- input$delete_flag
          flagName <- r$flagsTable[index, "flagName"]

          shinyWidgets::confirmSweetAlert(
            session = session,
            inputId = "confirmFlagDelete",
            title = "Confirm Deletion",
            text = paste0("Are you sure you want to delete the flag '", flagName, "'?"),
            type = "warning",
            btn_labels = c("Cancel", "Delete"),
            danger_mode = TRUE
          )
        })

        observeEvent(input$confirmFlagDelete, {
          req(input$delete_flag, input$confirmFlagDelete)
          if (isTRUE(input$confirmFlagDelete)) {
            index <- input$delete_flag
            r$flagsTable <- r$flagsTable[-index, ]
          }
        })

        # Edit flag using the dialog box
        observeEvent(input$edit_flag, {
          index <- input$edit_flag
          r$flagBeingEditedIndex <- index

          selectedFlag <- r$flagsTable[index, ]

          rf_formula_res$set_formula(selectedFlag$flagRule)

          showModal(shiny::modalDialog(
            shiny::tags$h4("Edit Flag"),
            shiny::textInput(ns("flagName_textinput"), "Flag Name", width = "100%", value = selectedFlag$flagName),
            colourpicker::colourInput(ns("flagColor_colorinput"), "Flag Color", value = selectedFlag$flagColor, palette = "limited"),
            mod_fct_dragAndDropFormula_ui(ns("flagFormula_formula")),
            shiny::tags$h4("Flag message:"),
            shiny::verbatimTextOutput(ns("flagToBeAdded_textoutput"), placeholder = TRUE),
            shiny::actionButton(ns("acceptFlag_actionButton"), "Save Changes"),
            shiny::modalButton("Cancel"),
            footer = NULL
          ))
        })


        #
        # Return the flag table
        #
        rf_flagsTable <- shiny::reactive({
            shiny::req(r$flagsTable)
            return(r$flagsTable)
        })

        #return(rf_flagsTable)
        return(list(r_flagstable=rf_flagsTable, r_roworder=flagTable_row_order))
    })
}
