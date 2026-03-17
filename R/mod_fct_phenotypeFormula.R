mod_fct_dragAndDropFormula_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),

    shiny::checkboxInput(
      inputId = ns("showAdvancedVars"),
      label = "Show advanced metrics (days/age/span)",
      value = FALSE
    ),
    shiny::tags$div(
      class = "formula-tip",
      "Tip: for rate-like scores, use count / spanYearsSafe, e.g. fractureCodes / fractureCodes (spanYearsSafe)."
    ),

    shiny::uiOutput(ns("operation_expression")),
    shiny::tags$style(shiny::HTML(sprintf("
  /* --- Formula builder layout --- */
      #%s .formula-card {
        border: 1px solid #e5e7eb;
        border-radius: 8px;
        padding: 12px;
        background: #fff;
      }

      #%s .formula-tip {
        font-size: 12px;
        color: #6b7280;
        margin: 2px 0 10px 0;
      }

      #%s .formula-drop-hint {
        font-size: 12px;
        color: #6b7280;
        margin: 6px 0 10px 0;
      }

      /* The drop zone is the orderInput container */
      #%s .jqui-orderInput {
        border: 2px dashed #9ca3af;
        border-radius: 10px;
        padding: 10px;
        background: #f9fafb;
        min-height: 56px;
      }

      /* Chips inside orderInput */
      #%s .jqui-orderInput .ui-state-default {
        border-radius: 8px;
        padding: 4px 8px;
        margin: 3px;
        font-size: 13px;
        background: #ffffff;
        border: 1px solid #d1d5db;
      }

      /* Source palettes look compact */
      #%s .jqui-orderInput-source {
        border: 1px solid #e5e7eb;
        border-radius: 8px;
        padding: 8px;
        background: #fff;
      }

      #%s .palette-title {
        font-weight: 600;
        margin: 10px 0 6px 0;
        font-size: 12px;
        color: #374151;
        text-transform: uppercase;
        letter-spacing: .02em;
      }",  ns("operation_expression"),
          ns("operation_expression"),
          ns("operation_expression"),
          ns("operation_expression"),
          ns("operation_expression"),
          ns("operation_expression"),
          ns("operation_expression"))))
  )
}

mod_fct_dragAndDropFormula_server <- function(id, r_groupedCovariates, operatorItems, titleText, placeholder, variableItems = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    destBoxes <- shiny::reactiveVal(NULL)

    # group items
    r_groupItems_all <- shiny::reactive({
      shiny::req(r_groupedCovariates$groupedCovariatesTibble)

      if (!is.null(variableItems)) {
        gi <- if (shiny::is.reactive(variableItems)) variableItems() else variableItems
      } else {
        gi <- setNames(
          r_groupedCovariates$groupedCovariatesTibble$groupId,
          r_groupedCovariates$groupedCovariatesTibble$groupName
        )
      }

      gi <- unlist(gi)
      if (is.null(names(gi)) || any(!nzchar(names(gi)))) {
        stop("groupItems must be a named vector: names=labels, values=tokens")
      }
      gi
    })

    # filtered list for the CODE GROUPS source buttons
    r_groupItems <- shiny::reactive({
      gi <- r_groupItems_all()
      show_adv <- isTRUE(input$showAdvancedVars)

      if (!show_adv) {
        gi <- gi[grepl("^g\\d+$", gi)]
      }
      gi
    })

    # counts-only group ids (gX)
    r_groupItems_counts <- shiny::reactive({
      gi <- r_groupItems_all()
      gi[grepl("^g\\d+$", gi)]
    })

    # advanced-only group ids (gX_daysToFirst etc)
    r_groupItems_advanced <- shiny::reactive({
      gi <- r_groupItems_all()
      gi[grepl("^g\\d+_(daysToFirst|daysToLast|ageFirst|spanDaysRaw|spanDaysSafe|spanYearsSafe)$", gi)]
    })

    output$operation_expression <- shiny::renderUI({
      shiny::req(r_groupedCovariates$groupedCovariatesTibble)

      show_adv <- isTRUE(input$showAdvancedVars)

      groupItems_counts <- r_groupItems_counts()
      groupItems_advanced <- r_groupItems_advanced()

      numbersItems <- 0:9

      htmltools::tags$div(
        class = "formula-card",

        htmltools::tags$div(class = "palette-title", titleText),

        htmltools::tags$div(
          class = "formula-drop-hint",
          "Drag items from below into the dashed area to build the formula."
        ),

        shinyjqui::orderInput(
          inputId = ns("dest_boxes"),
          width = "100%",
          label = NULL,
          items = {
            gi_all <- r_groupItems_all()

            toks <- destBoxes()
            if (is.null(toks)) {
              toks <- input$dest_boxes
            }

            if (is.null(toks) || length(toks) == 0) {
              NULL
            } else {
              labels <- vapply(toks, function(tok) {
                idx <- match(tok, unname(gi_all))
                if (!is.na(idx)) names(gi_all)[idx] else tok
              }, character(1))
              stats::setNames(toks, labels)
            }
          },
          placeholder = placeholder
        ),

        htmltools::tags$div(
          style = "display:flex; gap:8px; margin-bottom:8px;",
          shiny::actionButton(ns("clearFormula"), "Clear formula", class = "btn btn-outline-secondary btn-sm")
        ),


        htmltools::tags$div(class = "palette-title", "Code groups (counts)"),
        shinyjqui::orderInput(
          inputId = ns("source_boxes_counts"),
          width = "100%",
          label = NULL,
          items = groupItems_counts,
          as_source = TRUE,
          connect = ns("dest_boxes")
        ),

        shiny::conditionalPanel(
          condition = sprintf("input['%s'] === true", ns("showAdvancedVars")),
          htmltools::tags$div(class = "palette-title", "Advanced metrics (days/age/span)"),
          shinyjqui::orderInput(
            inputId = ns("source_boxes_advanced"),
            width = "100%",
            label = NULL,
            items = groupItems_advanced,
            as_source = TRUE,
            connect = ns("dest_boxes")
          )
        ),

        htmltools::tags$div(class = "palette-title", "Operators"),
        shinyjqui::orderInput(
          inputId = ns("source_boxes_operators"),
          width = "100%",
          label = NULL,
          items = operatorItems,
          as_source = TRUE,
          connect = ns("dest_boxes")
        ),

        htmltools::tags$div(class = "palette-title", "Numbers"),
        shinyjqui::orderInput(
          inputId = ns("source_boxes_numbers"),
          width = "100%",
          label = NULL,
          items = numbersItems,
          as_source = TRUE,
          connect = ns("dest_boxes")
        )
      )
    })

    # Keep destBoxes always in sync with what user has dragged
    shiny::observeEvent(input$dest_boxes, {
      if (is.null(input$dest_boxes) || length(input$dest_boxes) == 0) {
        destBoxes(character(0))
      } else if (identical(input$dest_boxes, placeholder)) {
        destBoxes(character(0))
      } else {
        destBoxes(input$dest_boxes)
      }
    }, ignoreInit = TRUE)

    # clean formula at once
    shiny::observeEvent(input$clearFormula, {
      destBoxes(character(0))
    })


    rf_formula <- shiny::reactive({
      shiny::req(r_groupedCovariates$groupedCovariatesTibble)

      expr <- destBoxes()
      shiny::req(!is.null(expr), length(expr) > 0)
      shiny::req(!identical(expr, placeholder))

      gi_all <- r_groupItems_all()

      expressionNames <- vapply(expr, function(tok) {
        idx <- match(tok, unname(gi_all))
        if (!is.na(idx)) names(gi_all)[idx] else tok
      }, character(1))

      list(
        formula = paste(expr, collapse = ""),
        formulaPretty = paste(expressionNames, collapse = "")
      )
    })


    set_formula <- function(formula_string = NULL) {
      if (is.null(formula_string)) {
        parsed <- NULL
      } else {
        parsed <- stringr::str_extract_all(
          formula_string,
          "g\\d+_(?:daysToFirst|daysToLast|ageFirst|spanDaysRaw|spanDaysSafe|spanYearsSafe)|g\\d+|[><=()!&|+\\-*/]+|\\d+\\.?\\d*"
        )[[1]]
      }
      destBoxes(parsed)
    }

    list(
      get_formula = rf_formula,
      set_formula = set_formula
    )
  })
}
