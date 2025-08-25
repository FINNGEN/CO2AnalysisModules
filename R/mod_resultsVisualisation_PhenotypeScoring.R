#' @title CodeWAS Results Visualization UI
#' @description UI module for visualizing CodeWAS results. This module provides controls to customize the appearance of the plot and options to download the plot and data.
#'
#' @param id A string representing the module's namespace.
#'
#' @return A Shiny UI element that can be included in a Shiny app.
#'
#'
#' @export
#'
mod_resultsVisualisation_PhenotypeScoring_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    title = "Phenotype Scoring Results",
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    shiny::tagList(
      shiny::h4("CodeWAS Results Table"),
      reactable::reactableOutput(ns("codeWasCovariatesTable"), height = 500),
      shiny::actionButton(
        ns("createGroupFromSelected"),
        "Create Group From Selected"
      ),
      shiny::hr(),
      shiny::hr(),
      shiny::hr(),
      shiny::h4("Code Groups"),
      reactable::reactableOutput(ns("groupedCovariatesTable"), height = 500),
      # shiny::hr(),
      # shiny::hr(),
      # shiny::hr(),
      # shiny::h4("Groups Overlap"),
      # shiny::plotOutput(ns("groupsOverlapPlot"), height = 500),
      shiny::hr(),
      shiny::hr(),
      shiny::hr(),
      shiny::h4("Formula:"),
      mod_fct_dragAndDropFormula_ui(ns("totalScoreFormula_formula")),
      shiny::tags$h4("Formula message:"),
      shiny::verbatimTextOutput(ns("totalScoreFormula_text"), placeholder = TRUE),
      shiny::hr(),
      shiny::hr(),
      shiny::hr(),
      shiny::h4("Flags:"),
      mod_fct_phenotypeFlags_ui(ns("phenotypeFlags_flags")),
      shiny::br(), shiny::br(), shiny::br(),
      shiny::tags$h4("Flags message:"),
      shiny::verbatimTextOutput(ns("phenotypeFlags_text"), placeholder = TRUE),
      shiny::hr(),
      shiny::hr(),
      shiny::hr(),
      shiny::h4("Total Score Distribution"),
      shiny::plotOutput(ns("totalScoreDistributionPlot"), height = 500),
      shiny::sliderInput(
        ns("scoreRange"),
        "Score Range",
        width = "100%",
        min = 0,
        max = 10, # This will be updated dynamically
        value = c(0, 10),
        step = 1
      ),
      shiny::textOutput(ns("selectedPatientsCount")),
      shiny::downloadButton(
        ns("exportSelectedSubjects"),
        "Export Selected Subjects"
      ),
      shiny::hr(),
      shiny::hr(),
      tags$script(HTML("
      Shiny.addCustomMessageHandler('focusInput', function(message) {
        var id = message.id;
        setTimeout(function() {
          var el = document.getElementById(id);
          if(el) {
            el.focus();
            // Optionally select all text:
            if(el.select) el.select();
          }
        }, 200);  // small delay to ensure modal is rendered
      });
    "))
    )
  ) # end of fluidPage
}


#' @title CodeWAS Results Visualization Server
#' @description Server module for handling the logic of the CodeWAS results visualization UI. This module creates interactive plots and tables based on the analysis results and allows the plot and data to be downloaded.
#'
#' @param id A string representing the module's namespace.
#' @param analysisResults Pooled connection to the analysis results duckdb.
#'
#'
#' @return The module returns server-side logic to generate and manage the CodeWAS results visualization.
#'
#'
#' @export
mod_resultsVisualisation_PhenotypeScoring_server <- function(id, analysisResults) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    emptyGroupedCovariatesTibble <- tibble::tibble(
      groupId = character(),
      groupName = character(),
      covariateIds = list(),
      conceptCodes = list(),
      covariateNames = list(),
      covariatesDistribution = list()
    )

    r <- shiny::reactiveValues(
      codeWasCovariatesTibble = NULL,
      errorMessageTotalScore = NULL,
      errorMessagePhenotypeFlags = NULL
    )

    # Break the groupedCovariatesPerPersonTibble into columns to avoid loop
    r_groupedCovariates <- shiny::reactiveValues(
      groupedCovariatesTibble = emptyGroupedCovariatesTibble,
      groupedCovariatesPerPersonTibble = NULL,
      groupedCovariatesPerPersonTibble_totalScore = NULL,
      groupedCovariatesPerPersonTibble_flag = NULL
    )

    #
    # Start up: get the list of codes from database into r$codeWasCovariatesTibble
    #
    shiny::observe({
      r$codeWasCovariatesTibble <- .getcodeWasCovariatesTibble(analysisResults)
    })

    #
    # When r$codeWasCovariatesTibble is ready, plot it
    #
    output$codeWasCovariatesTable <- reactable::renderReactable({
      shiny::req(r$codeWasCovariatesTibble)

      toPlot <- r$codeWasCovariatesTibble |>
        dplyr::transmute(
          domainId = domainId,
          vocabularyId = vocabularyId,
          conceptCode = conceptCode,
          covariateName = stringr::str_remove(covariateName, ".*:"),
          nCasesYes = nCasesYes,
          mplog = -log10(pValue),
          beta = log(oddsRatio),
          isDataAvailable = isDataAvailable
        )

      columns <- list(
        domainId = reactable::colDef(name = "Domain", minWidth = 40),
        vocabularyId = reactable::colDef(name = "Vocabulary", minWidth = 40),
        conceptCode = reactable::colDef(name = "Concept Code", minWidth = 40),
        covariateName = reactable::colDef(name = "Covariate Name", minWidth = 200),
        nCasesYes = reactable::colDef(name = "N Cases", minWidth = 40),
        mplog = reactable::colDef(name = "mplog", minWidth = 40, format = reactable::colFormat(digits = 2)),
        beta = reactable::colDef(name = "beta", minWidth = 40, format = reactable::colFormat(digits = 2)),
        isDataAvailable = reactable::colDef(name = "Data Available", minWidth = 40)
      )

      reactable::reactable(toPlot,
        columns = columns,
        filterable = TRUE,
        sortable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        pagination = TRUE,
        selection = "multiple",
        onClick = "select",
        defaultSorted = list("mplog" = "desc")
      )
    })


    #
    # When click input$createGroupFromSelected, create a new group into r_groupedCovariates
    #
    # shiny::observeEvent(input$createGroupFromSelected, {
    #   selected <- reactable::getReactableState("codeWasCovariatesTable", "selected")
    #   if (!is.null(selected)) {
    #     # Get the selected rows from the table
    #     selectedRows <- r$codeWasCovariatesTibble[selected, ]
    #
    #     # Update the list of groups with selected rows
    #     res <- .appendCovariateGroup(
    #       analysisResults = analysisResults,
    #       covariateIds = selectedRows$covariateId,
    #       groupedCovariatesTibble = r_groupedCovariates$groupedCovariatesTibble,
    #       groupedCovariatesPerPersonTibble = r_groupedCovariates$groupedCovariatesPerPersonTibble
    #     )
    #     r_groupedCovariates$groupedCovariatesTibble <- res$groupedCovariatesTibble
    #     r_groupedCovariates$groupedCovariatesPerPersonTibble <- res$groupedCovariatesPerPersonTibble
    #   }
    #
    #   # clear selection
    #   reactable::updateReactable("codeWasCovariatesTable", selected = NA)
    # })

    shiny::observeEvent(input$createGroupFromSelected, {
      selected <- reactable::getReactableState("codeWasCovariatesTable", "selected")
      if (is.null(selected) || length(selected) == 0) {
        showNotification("Please select at least one row to group.", type = "error")
        return()
      }

      showModal(
        modalDialog(
          title = "Name Your Group",
          textInput(ns("groupNameInput"), "Group Name:", ""),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirmGroupName"), "Create Group")
          ),
          easyClose = TRUE,
          fade = TRUE,
          size = "s"
        )
      )
      session$sendCustomMessage(
        type = 'focusInput',
        message = list(id = ns("groupNameInput"))
      )

    })

    shiny::observeEvent(input$confirmGroupName, {
      groupName <- input$groupNameInput

      # Validate group name input
      if (is.null(groupName) || nchar(trimws(groupName)) == 0) {
        showNotification("Group name cannot be empty.", type = "error")
        return()
      }

      selected <- reactable::getReactableState("codeWasCovariatesTable", "selected")
      if (is.null(selected) || length(selected) == 0) {
        showNotification("No rows selected.", type = "error")
        removeModal()
        return()
      }

      selectedRows <- r$codeWasCovariatesTibble[selected, ]

      res <- .appendCovariateGroup(
        analysisResults = analysisResults,
        covariateIds = selectedRows$covariateId,
        newGroupName = groupName,
        groupedCovariatesTibble = r_groupedCovariates$groupedCovariatesTibble,
        groupedCovariatesPerPersonTibble = r_groupedCovariates$groupedCovariatesPerPersonTibble
      )

      r_groupedCovariates$groupedCovariatesTibble <- res$groupedCovariatesTibble
      r_groupedCovariates$groupedCovariatesPerPersonTibble <- res$groupedCovariatesPerPersonTibble

      reactable::updateReactable("codeWasCovariatesTable", selected = NA)
      removeModal()
    })

    #
    # When r_groupedCovariates$groupedCovariatesTibble is ready, plot table of groups
    #
    output$groupedCovariatesTable <- reactable::renderReactable({
      toPlot <- r_groupedCovariates$groupedCovariatesTibble |>
        dplyr::mutate(editButton = NA,deleteButton = NA)

      columns <- list(
        groupId = reactable::colDef(show = FALSE),
        groupName = reactable::colDef(name = "Group Name", minWidth = 50),
        covariateIds = reactable::colDef(show = FALSE),
        conceptCodes = reactable::colDef(
          name = "Concept Codes",
          minWidth = 100,
          cell = function(value) {
            display <- if (length(value) > 3) {
              paste(c(value[1:3], "..."), collapse = "<br>")
            } else {
              paste(value, collapse = "<br>")
            }
            full <- paste(value, collapse = ", ")
            as.character(htmltools::tags$div(title = full, HTML(display),
              style = "transition: background-color 0.3s ease;",
              onmouseover = "this.style.backgroundColor='#ffffcc'; this.style.cursor='pointer';",
              onmouseout = "this.style.backgroundColor='';"))
          },
          html = TRUE
        ),
        covariateNames = reactable::colDef(
          name = "Covariate Names",
          minWidth = 200,
          cell = function(value) {
            processed <- value |>
              stringr::str_remove(".*:") |>
              stringr::str_trunc(80)

            display <- if (length(processed) > 3) {
              paste(c(processed[1:3], "..."), collapse = "<br>")
            } else {
              paste(processed, collapse = "<br>")
            }
            full <- paste(processed, collapse = ", ")
            as.character(htmltools::tags$div(title = full, HTML(display),
               style = "transition: background-color 0.3s ease;",
               onmouseover = "this.style.backgroundColor='#ffffcc'; this.style.cursor='pointer';",
               onmouseout = "this.style.backgroundColor='';"))
          },
          html = TRUE
        ),
        covariatesDistribution = reactable::colDef(
          name = "Covariates Distribution",
          width = 300,
          cell = function(value, index) {
            htmltools::tagList(
              .renderCovariatesDistribution(value),
              shiny::actionButton(paste0("showDistPlot_", index), "View Larger", style = "font-size: 0.7em; margin-top: 5px;")
            )
          }
        ),
        editButton = reactable::colDef(
          name = "",
          sortable = FALSE,
          cell = function(value, index) {
            htmltools::tags$button(
              shiny::icon("pen"),
              class = "btn btn-outline-primary btn-sm",
              onclick = sprintf(
                "Shiny.setInputValue('%s', %d, {priority: 'event'})",
                ns("edit_row"), index
              )
            )
          },
          maxWidth = 50
        ),
        deleteButton = reactable::colDef(
          name = "",
          sortable = FALSE,
          cell = function(value, index) {
            htmltools::tags$button(
              shiny::icon("trash"),
              class = "btn btn-outline-danger btn-sm",
              onclick = sprintf(
                "Shiny.setInputValue('%s', %d, {priority: 'event'})",
                ns("delete_row"), index
              )
            )
          },
          maxWidth = 50
        )
      )

      reactable::reactable(toPlot,
        columns = columns,
        resizable = TRUE
      )
    })

    observeEvent(input$delete_row, {
      index <- input$delete_row
      groupName <- r_groupedCovariates$groupedCovariatesTibble[index, "groupName"]

      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = "confirmDelete",
        title = "Confirm Deletion",
        text = paste0("Are you sure you want to delete the group '", groupName, "'?"),
        type = "warning",
        btn_labels = c("Cancel", "Delete"),
        danger_mode = TRUE
      )
    })

    observeEvent(input$confirmDelete, {
      req(input$delete_row, input$confirmDelete)
      if (isTRUE(input$confirmDelete)) {
        index <- input$delete_row

        res <- .deleteCovariateGroup(
          rowIndex = index,
          groupedCovariatesTibble = r_groupedCovariates$groupedCovariatesTibble,
          groupedCovariatesPerPersonTibble = r_groupedCovariates$groupedCovariatesPerPersonTibble
        )

        r_groupedCovariates$groupedCovariatesTibble <- res$groupedCovariatesTibble
        r_groupedCovariates$groupedCovariatesPerPersonTibble <- res$groupedCovariatesPerPersonTibble
      }
    })


    observeEvent(input$edit_row, {
      index <- input$edit_row
      currentName <- r_groupedCovariates$groupedCovariatesTibble[index, "groupName"]

      shinyWidgets::inputSweetAlert(
        session = session,
        inputId = "confirmEdit",
        title = paste0("Edit name for group '", currentName, "'"),
        input = "text",
        inputValue = currentName,
        showCancelButton = TRUE,
        inputPlaceholder = "Enter new group name",
        type = "question"
      )

    })

    observeEvent(input$confirmEdit, {
      req(input$edit_row, input$confirmEdit)

      index <- input$edit_row
      newName <- input$confirmEdit

      r_groupedCovariates$groupedCovariatesTibble[index, "groupName"] <- newName
    })

    #
    # When r$groupOfCovariatesObject is ready, plot the upset plot of groups
    #
    # output$groupsOverlapPlot <- shiny::renderPlot({
    #   shiny::req(r$groupOfCovariatesObject$groupsTibble |> nrow() > 0)

    #   columnNames <- r$groupOfCovariatesObject$personGroupsTibble |>
    #     names() |>
    #     setdiff(c("personSourceValue", "total", "totalBin"))

    #   r$groupOfCovariatesObject$personGroupsTibble |>
    #     dplyr::mutate(dplyr::across(columnNames, ~ ifelse(.x == 0, NA, paste("Group", dplyr::cur_column())))) |>
    #     dplyr::filter(!dplyr::if_all(columnNames, is.na)) |>
    #     dplyr::mutate(groups = purrr::pmap(.l = dplyr::across(columnNames), .f = ~ na.omit(c(...)))) |>
    #     ggplot2::ggplot(aes(x = groups)) +
    #     ggplot2::geom_bar() +
    #     ggplot2::geom_text(stat = "count", aes(label = ggplot2::after_stat(count)), vjust = -1) +
    #     ggupset::scale_x_upset(n_intersections = 20) +
    #     ggplot2::theme_minimal()
    # })

    #
    # render the flag formula builder
    #
    rf_totalScoreFormula <- mod_fct_dragAndDropFormula_server(
      id = "totalScoreFormula_formula",
      r_groupedCovariates = r_groupedCovariates,
      operatorItems = c(
        `(` = "(", `)` = ")",
        `+` = "+",
        `-` = "-",
        `*` = "*",
        `/` = "/"
      ),
      placeholder = "Drag and Drop here to create formula"
    )


    #
    # when input$formula is changed, attempt to use it
    # if it is valid, calculate totalScore and totalScoreBin columns
    # if it is invalid, show error message and delete totalScore and totalScoreBin columns
    #
    shiny::observe({
      shiny::req(rf_totalScoreFormula())

      totalScoreFormula <- rf_totalScoreFormula()
      formula <- totalScoreFormula$formula

      groupedCovariatesPerPersonTibble <- r_groupedCovariates$groupedCovariatesPerPersonTibble

      errorMessage <- NULL
      groupedCovariatesPerPersonTibble_totalScore <- NULL

      # Check if formula is potentially incomplete (e.g., ends with operator or is empty)
      isIncomplete <- function(f) {
        if (!nzchar(f)) return(TRUE)
        endsWithOp <- grepl("[+*/\\-]\\s*$", f)
        unbalancedParens <- stringr::str_count(f, "\\(") != stringr::str_count(f, "\\)")
        endsWithOp || unbalancedParens
      }

      if (isIncomplete(formula)) {
        r$errorMessageTotalScore <- NULL
        r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore <- NULL
        return()
      }

      tryCatch(
        {
          groupedCovariatesPerPersonTibble_totalScore <- .calculateTotalScores(
            groupedCovariatesPerPersonTibble = groupedCovariatesPerPersonTibble,
            formula = totalScoreFormula$formula
          )
        },
        error = function(e) {
          errorMessage <<- "Error: Invalid formula. Please check your expression syntax and variable names."
        }
      )

      if (is.null(errorMessage)) {
        # show formula
        r$errorMessageTotalScore <- totalScoreFormula$formulaPretty
      } else {
        r$errorMessageTotalScore <- errorMessage
      }

      r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore <- groupedCovariatesPerPersonTibble_totalScore
    })

    #
    # When r$errorMessageTotalScore is ready, update the formula text
    #
    output$totalScoreFormula_text <- shiny::renderText({
      shiny::req(r$errorMessageTotalScore)
      r$errorMessageTotalScore
    })

    #
    # render the flag formula builder
    #
    rf_flagsTable <- mod_fct_phenotypeFlags_server("phenotypeFlags_flags", r_groupedCovariates)

    #
    # Evaluate the flag formulas
    #
    shiny::observe({
      shiny::req(rf_flagsTable())
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)

      flagsTable <- rf_flagsTable()

      # Check if flagsTable has any rows
      if (nrow(flagsTable) == 0) {
        r$errorMessagePhenotypeFlags <- NULL
        r_groupedCovariates$groupedCovariatesPerPersonTibble_flag <- NULL
        return()
      }

      flagsTable <- flagsTable |>
        dplyr::mutate(flagCaseWhenRule = paste0(flagRule, " ~ '", flagName, "'"))
      flagCaseWhenRules <- paste(flagsTable$flagCaseWhenRule, collapse = ", \n")

      groupedCovariatesPerPersonTibble <- r_groupedCovariates$groupedCovariatesPerPersonTibble

      errorMessage <- NULL
      groupedCovariatesPerPersonTibble_flag <- NULL
      tryCatch(
        {
          groupedCovariatesPerPersonTibble_flag <- eval(parse(text = paste(
            "groupedCovariatesPerPersonTibble |>",
            "dplyr::mutate(flag = dplyr::case_when(", flagCaseWhenRules, ", TRUE ~ 'no-flag'))"
          )))

          groupedCovariatesPerPersonTibble_flag <- groupedCovariatesPerPersonTibble_flag |>
            dplyr::select(personSourceValue, flag)
        },
        error = function(e) {
          errorMessage <<- e$message
        }
      )

      if (is.null(errorMessage)) {
        r$errorMessagePhenotypeFlags <- errorMessage
      } else {
        r$errorMessagePhenotypeFlags <- paste("error when filtering with flag:", errorMessage)
      }

      r_groupedCovariates$groupedCovariatesPerPersonTibble_flag <- groupedCovariatesPerPersonTibble_flag
    })

    #
    # When r$errorMessagePhenotypeFlags is ready, update the formula text
    #
    output$phenotypeFlags_text <- shiny::renderText({
      shiny::req(r$errorMessagePhenotypeFlags)
      r$errorMessagePhenotypeFlags
    })


    #
    # When r_groupedCovariates is ready, update the slider range
    #
    shiny::observe({
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble |> nrow() > 0)
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)

      groupedCovariatesPerPersonTibble <- r_groupedCovariates$groupedCovariatesPerPersonTibble |>
        dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore, by = "personSourceValue")

      shiny::updateSliderInput(
        session,
        "scoreRange",
        min = 0,
        max = max(groupedCovariatesPerPersonTibble$totalScoreBin |> as.integer(), na.rm = TRUE),
        value = c(0, max(groupedCovariatesPerPersonTibble$totalScoreBin |> as.integer(), na.rm = TRUE))
      )
    })

    #
    # When r_groupedCovariates is ready or slider is changed, plot the total score distribution, if flag is available, add flag to the plot
    #
    output$totalScoreDistributionPlot <- shiny::renderPlot({
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble |> nrow() > 0)
      shiny::req(input$scoreRange)
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)

      groupedCovariatesPerPersonTibble <- r_groupedCovariates$groupedCovariatesPerPersonTibble |>
        dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore, by = "personSourceValue")

      if (!is.null(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag)) {
        groupedCovariatesPerPersonTibble <- groupedCovariatesPerPersonTibble |>
          dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag, by = "personSourceValue")
      } else {
        groupedCovariatesPerPersonTibble <- groupedCovariatesPerPersonTibble |>
          dplyr::mutate(flag = "no-flag")
      }

      flagsTable <- rf_flagsTable()
      flagsTable <- flagsTable |>
        dplyr::bind_rows(tibble::tibble(flagName = "no-flag", flagColor = "grey"))

      # Create the plot
      p <- groupedCovariatesPerPersonTibble |>
        ggplot2::ggplot(aes(x = totalScoreBin, fill = flag)) +
        ggplot2::geom_bar(position = "stack") +
        ggplot2::theme_minimal() +
        ggplot2::scale_fill_manual(
          values = setNames(flagsTable$flagColor, flagsTable$flagName)
        )

      # Add box overlay if slider values are set
      if (!is.null(input$scoreRange)) {
        p <- p +
          ggplot2::annotate(
            "rect",
            xmin = input$scoreRange[1],
            xmax = input$scoreRange[2],
            ymin = -Inf,
            ymax = Inf,
            alpha = 0.2,
            fill = "blue"
          )
      }

      return(p)
    })

    #
    # When r$groupOfCovariatesObject is ready or slider is changed, update the selected patients count
    #
    output$selectedPatientsCount <- shiny::renderText({
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble |> nrow() > 0, input$scoreRange)
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)

      groupedCovariatesPerPersonTibble <- r_groupedCovariates$groupedCovariatesPerPersonTibble |>
        dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore, by = "personSourceValue")

      # Count subjects in the selected range
      nSelected <- groupedCovariatesPerPersonTibble |>
        dplyr::filter(totalScoreBin |> as.integer() >= input$scoreRange[1] & totalScoreBin |> as.integer() <= input$scoreRange[2]) |>
        nrow()

      paste("Number of patients selected:", nSelected)
    })

    #
    # When input$exportSelectedSubjects is clicked, export subjects with total score in the range of the slider
    #
    output$exportSelectedSubjects <- shiny::downloadHandler(
      filename = function() {
        paste0("selected_subjects_", input$scoreRange[1], "_to_", input$scoreRange[2], ".csv")
      },
      content = function(file) {
        shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble |> nrow() > 0)
        shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)

        groupedCovariatesPerPersonTibble <- r_groupedCovariates$groupedCovariatesPerPersonTibble |>
          dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore, by = "personSourceValue")

        if (is.null(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag)) {
          groupedCovariatesPerPersonTibble <- groupedCovariatesPerPersonTibble |>
            dplyr::mutate(flag = "no-flag")
        } else {
          groupedCovariatesPerPersonTibble <- groupedCovariatesPerPersonTibble |>
            dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag, by = "personSourceValue")
        }

        # Get the subjects with total score in the range of the slider
        selectedSubjects <- groupedCovariatesPerPersonTibble |>
          dplyr::filter(totalScoreBin |> as.integer() >= input$scoreRange[1] & totalScoreBin |> as.integer() <= input$scoreRange[2])

        # Write the selected subjects to the file
        write.csv(selectedSubjects, file, row.names = FALSE, na = "")
      }
    )
  })
}



#' Get All Covariates Tibble
#' @description Retrieves all covariates data from analysis results and joins with reference tables
#' @param analysisResults A database connection containing analysis results tables
#' @return A tibble containing all covariates data with reference information
#' @importFrom dplyr tbl left_join filter distinct mutate collect
#' @importFrom stats na.omit
.getcodeWasCovariatesTibble <- function(analysisResults) {

  # TEMP this will be moved to the fct_PhenotypeScoring.R
  codewasResultsTibble <- analysisResults |>
    dplyr::tbl("codewasResults") |>
    dplyr::left_join(analysisResults |> dplyr::tbl("covariateRef"), by = c("covariateId" = "covariateId")) |>
    dplyr::left_join(analysisResults |> dplyr::tbl("analysisRef"), by = c("analysisId" = "analysisId")) |>
    # TEMP
    dplyr::filter(!(vocabularyId == "ATC" & nchar(conceptCode) < 7)) |> 
    dplyr::collect()

    covariatesPerPersonTibble <- analysisResults |> dplyr::tbl("covariatesPerPerson") |>
        dplyr::distinct(covariateId) |>
        dplyr::mutate(isDataAvailable = 1) |>
        dplyr::collect()

    includeDaysToFirstEvent <- any(c(151, 152) %in% (covariatesPerPersonTibble$covariateId %% 1000))

    if (includeDaysToFirstEvent) {
      codewasResultsTibble <- dplyr::bind_rows(
        codewasResultsTibble,
        codewasResultsTibble |>
          dplyr::filter(analysisId %in% c(141, 142)) |>
          dplyr::mutate(
            analysisId = analysisId + 10,
          covariateId = covariateId + 10,
          analysisName = paste0(analysisName, " (days to first event)"),
          covariateName = paste0(covariateName, " (days to first event)")
          )
      )
    }

    # END TEMP
  # END TEMP
    codewasResultsTibble <- codewasResultsTibble |>
      dplyr::left_join(covariatesPerPersonTibble, by = c("covariateId" = "covariateId")) |>
      dplyr::mutate(isDataAvailable = ifelse(is.na(isDataAvailable), FALSE, TRUE))

    return(codewasResultsTibble)
}


#' Append Covariate Group
#' @description Appends a new group of covariates to the groupOfCovariatesObject
#' @param analysisResults A database connection containing analysis results tables
#' @param covariateIds A vector of covariate ids
#' @param groupedCovariatesTibble A tibble containing the grouped covariates
#' @param groupedCovariatesPerPersonTibble A tibble containing the grouped covariates per person
#' @return A list containing the updated group of covariates object
#' @importFrom dplyr tbl left_join filter distinct mutate collect
.appendCovariateGroup <- function(
    analysisResults,
    covariateIds,newGroupName,
    groupedCovariatesTibble,
    groupedCovariatesPerPersonTibble) {

  if (nrow(groupedCovariatesTibble) == 0) {
    newGroupId <- 1
  } else {
    existingIds <- groupedCovariatesTibble$groupId
    existingNums <- as.integer(sub("g", "", existingIds))
    newGroupId <- max(existingNums, na.rm = TRUE) + 1
  }

  sumAllCovariatesPerPerson <- analysisResults |>
    dplyr::tbl("covariatesPerPerson") |>
    dplyr::distinct(personSourceValue) |>
    dplyr::left_join(
      analysisResults |>
        dplyr::tbl("covariatesPerPerson") |>
        dplyr::filter(covariateId %in% covariateIds) |>
        dplyr::group_by(personSourceValue) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop"),
      by = "personSourceValue"
    ) |>
    dplyr::mutate(value = ifelse(is.na(value), 0, value)) |>
    dplyr::collect()

  covariatesDistribution <- sumAllCovariatesPerPerson |>
    dplyr::count(value, sort = TRUE)

  # create a new group
  conceptCodes <- analysisResults |>
    dplyr::tbl("covariateRef") |>
    dplyr::filter(covariateId %in% covariateIds) |>
    dplyr::pull(conceptCode)

  covariateNames <- analysisResults |>
    dplyr::tbl("covariateRef") |>
    dplyr::filter(covariateId %in% covariateIds) |>
    dplyr::pull(covariateName)

  groupTibble <- tibble::tibble(
    groupId = paste0("g", newGroupId),
    groupName = newGroupName,
    covariateIds = list(covariateIds),
    conceptCodes = list(conceptCodes),
    covariateNames = list(covariateNames),
    covariatesDistribution = list(covariatesDistribution)
  )

  sumAllCovariatesPerPerson <- sumAllCovariatesPerPerson |>
    dplyr::rename(!!paste0("g", newGroupId) := value)

  # append
  groupedCovariatesTibble <- dplyr::bind_rows(groupedCovariatesTibble, groupTibble)
  if (is.null(groupedCovariatesPerPersonTibble)) {
    groupedCovariatesPerPersonTibble <- sumAllCovariatesPerPerson
  } else {
    groupedCovariatesPerPersonTibble <- dplyr::left_join(groupedCovariatesPerPersonTibble, sumAllCovariatesPerPerson, by = "personSourceValue")
  }

  return(list(
    groupedCovariatesTibble = groupedCovariatesTibble,
    groupedCovariatesPerPersonTibble = groupedCovariatesPerPersonTibble
  ))
}


.deleteCovariateGroup <- function(
    rowIndex,
    groupedCovariatesTibble,
    groupedCovariatesPerPersonTibble
) {
  # Get the groupId of the row to delete
  groupIdToDelete <- groupedCovariatesTibble$groupId[rowIndex]

  # Remove the row from groupedCovariatesTibble
  groupedCovariatesTibble <- groupedCovariatesTibble[-rowIndex, ]

  # Remove the corresponding column from groupedCovariatesPerPersonTibble if exists
  if (!is.null(groupedCovariatesPerPersonTibble)) {
    groupedCovariatesPerPersonTibble <- groupedCovariatesPerPersonTibble |>
      dplyr::select(-dplyr::any_of(groupIdToDelete))
  }

  return(list(
    groupedCovariatesTibble = groupedCovariatesTibble,
    groupedCovariatesPerPersonTibble = groupedCovariatesPerPersonTibble
  ))
}


#' Calculate Total Scores
#' @description Calculates the total scores for each person in the `groupedCovariatesPerPersonTibble` given a formula
#' @param groupedCovariatesPerPersonTibble A tibble containing the grouped covariates per person
#' @param formula A string containing the formula to calculate the total scores
#' @return A tibble containing the updated `groupedCovariatesPerPersonTibble`, with a new column `totalScore` and `totalScoreBin`
#' @importFrom dplyr tbl left_join filter distinct mutate collect
.calculateTotalScores <- function(
    groupedCovariatesPerPersonTibble,
    formula) {
  # Calculate total scores
  breaks <- c(
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
    12, 14, 16, 18,
    20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95,
    100, 120, 140, 160, 180,
    200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950,
    1000, 1200, 1400, 1600, 1800,
    2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000, 7500, 8000, 8500, 9000, 9500,
    10000, 12000, 14000, 16000, 18000,
    20000, 25000, 30000, 35000, 40000, 45000, 50000, 55000, 60000, 65000, 70000, 75000, 80000, 85000, 90000, 95000,
    100000, 120000, 140000, 160000, 180000,
    200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 900000, 950000
  )

  # Parse formula first to catch syntax errors
  parsed_formula <- tryCatch(
    parse(text = formula),
    error = function(e) stop(paste("Formula syntax error:", e$message))
  )

  groupedCovariatesPerPersonTibble_totalScore <- groupedCovariatesPerPersonTibble |>
    dplyr::mutate(totalScore = eval(parsed_formula)) |>
    dplyr::mutate(totalScoreBin = cut(totalScore, breaks = breaks, include.lowest = TRUE)) |>
    dplyr::select(personSourceValue, totalScore, totalScoreBin)

  return(groupedCovariatesPerPersonTibble_totalScore)
}

#' Render Covariates Distribution
#' @description Renders a plot of the covariates distribution
#' @param covariatesDistribution A tibble containing the covariates distribution
#' @return A plot of the covariates distribution
#' @importFrom apexcharter apex ax_chart ax_legend
.renderCovariatesDistribution <- function(covariatesDistribution) {
  if (is.null(covariatesDistribution) || nrow(covariatesDistribution) == 0) return(NULL)
  if (!all(c("value", "n") %in% colnames(covariatesDistribution))) return(NULL)

  # Calculate IQR boundaries for outliers
  Q1 <- quantile(covariatesDistribution$value, 0.25, na.rm = TRUE)
  Q3 <- quantile(covariatesDistribution$value, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lowerBound <- Q1 - 1.5 * IQR
  upperBound <- Q3 + 1.5 * IQR

  covariatesDistribution$isOutlier <- with(covariatesDistribution, ifelse(value < lowerBound | value > upperBound,"outlier_group_value",""))

  plot <- apexcharter::apex(
        covariatesDistribution,
        apexcharter::aes(x = value, y = n, fill = as.factor(isOutlier)),
        type = "column",
        height = 150,
        width = 250
      ) |>
        apexcharter::ax_colors(c("#3498DB", "#E74C3C")) |>
        apexcharter::ax_chart(toolbar = list(show = FALSE)) |>
        apexcharter::ax_xaxis(type = "numeric") |>
        apexcharter::ax_legend(show = FALSE)

  htmltools::tags$div(style = "width: 250px; height: 150px;", plot)
}
