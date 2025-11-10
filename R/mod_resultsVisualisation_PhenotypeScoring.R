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

      shiny::wellPanel(
        shiny::fluidRow(
          # --- Left: Title ---
          shiny::column(
            width = 4,
            shiny::h4("Code Groups Table")
          ),
          shiny::column(
            width = 4,
            align = "right",
            shiny::div(
              shiny::fileInput(
                ns("uploadGroupedCovariates"),
                label = "Import Code Groups",
                accept = c(".json",".tsv"),
                buttonLabel = "Import",
                placeholder = "Choose file...",
                width = "240px"
              )
            )
          ),
          shiny::column(
            width = 4,
            align = "right",
              shiny::selectInput(
                ns("export_trigger"),
                label = "Export Code Groups",
                choices = c(
                  "Select format" = "",
                  "JSON" = "json",
                  "TSV (wide)" = "tsv_wide",
                  "TSV (long)" = "tsv_long"
                ),
                selected = "",
                width = "200px"
            ),
            #  download button
            shiny::downloadButton(
              ns("downloadGroupedCovariates"),
              label = "Download"
            )
          )
        )
      ),

      reactable::reactableOutput(ns("groupedCovariatesTable"), height = "auto"),
      shiny::hr(),
      # shiny::hr(),
      # shiny::hr(),
      # shiny::hr(),
      # shiny::h4("Groups Overlap"),
      # shiny::plotOutput(ns("groupsOverlapPlot"), height = 500),
      shiny::wellPanel(
      style = "padding: 15px; background-color: #f8f9fa;",
      shiny::h4("Formula:"),
      mod_fct_dragAndDropFormula_ui(ns("totalScoreFormula_formula")),
      shiny::hr(),
      shiny::tags$h4("Formula message:"),
      shiny::verbatimTextOutput(ns("totalScoreFormula_text"), placeholder = TRUE)),
      shiny::hr(),
      shiny::fluidRow(
        column(
          width = 7,
          # Left column: plots and slider
          tabsetPanel(
            id = ns("scorePlotTabs"),
            tabPanel("Total Score Bar Plot", plotly::plotlyOutput(ns("totalScoreDistributionPlot"), height = "400px")),
            tabPanel("Density Plot", plotly::plotlyOutput(ns("totalScoreDensityPlot"), height = "400px")),
            tabPanel("Upset Plot", shiny::plotOutput(ns("upsetPlot"), height = "400px",width = "auto")),
            tabPanel("Score Table", DT::dataTableOutput(ns("totalScoreTable")))
          ),
          shiny::br(), shiny::hr(),
          shiny::uiOutput(ns("selectedPatientsCount")),
          shiny::sliderInput(
            ns("scoreRange"),
            "Score Range",
            width = "100%",
            min = 0,
            max = 10, # to be updated dynamically
            value = c(0, 10),
            step = 1,
            ticks = T
          )
        ),
        column(
          width = 5,
          shiny::hr(),
          shiny::hr(),
          shiny::wellPanel(
          shiny::h4("Flags:"),
          shiny::hr(),
          mod_fct_phenotypeFlags_ui(ns("phenotypeFlags_flags")),
          shiny::br(),
          shiny::tags$h4(""), # No need for message about the possible phenotype flag errors
          shiny::verbatimTextOutput(ns("phenotypeFlags_text"), placeholder = TRUE),
          shiny::hr(),
          shiny::hr(),
          shiny::h4("Download Data:"),
          shiny::selectInput(
            ns("downloadFlagSelection"),
            "Selection for Download:",
            choices = c("All Data"),
            selected = "All Data"
          ),
          shiny::downloadButton(
            ns("exportSelectedSubjects"),
            "Export Selected Subjects"
          )
         )
        )
      ),
      shiny::hr(),
      shiny::hr(),

      # JavaScript handler for automatic download
      shiny::tags$script(HTML("
        Shiny.addCustomMessageHandler('triggerDownload', function(message) {
          const id = message.id;
          const btn = document.getElementById(id);
          if (btn) btn.click();
        });
      ")),

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
    ")),

      # Custom CSS to remove grey background and border from verbatimTextOutput
      shiny::tags$style(HTML(sprintf("
      #%s, #%s {
        background-color: transparent !important;
        border: none !important;
        padding: 0 !important;
        font-family: monospace;
        white-space: pre-wrap;
      }
    ", ns("totalScoreFormula_text"), ns("phenotypeFlags_text"))))
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
#' @importFrom stats mad median quantile
#' @importFrom utils write.table
#' @importFrom DT datatable
#' @importFrom plotly plot_ly ggplotly
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

    # track total score range
    rv_scoreRanges <- shiny::reactiveValues()

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
        nCasesYes = reactable::colDef(name = "N Cases", minWidth = 40,filterable = TRUE,
                                      filterMethod = .numericRangeFilter),
        mplog = reactable::colDef(name = "mplog", minWidth = 40,
                                  filterable = TRUE,
                                  filterMethod = .numericRangeFilter,
                                  format = reactable::colFormat(digits = 2)),
        beta = reactable::colDef(name = "beta", minWidth = 40,
                                 filterable = TRUE,
                                 filterMethod = .numericRangeFilter,
                                 format = reactable::colFormat(digits = 2)),
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
            plotDiv <- .renderCovariatesDistribution(value)
            htmltools::tags$div(
              onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("showDistPlot"), index),
              style = "cursor:pointer;",
              plotDiv
            )

          },
          html = TRUE
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


    observeEvent(input$showDistPlot, {

      idx <- input$showDistPlot
      dist_data <- r_groupedCovariates$groupedCovariatesTibble$covariatesDistribution[[idx]]
      values_expanded <- rep(dist_data$value, dist_data$n)

      modal_inputs <- reactiveValues(
        method = "iqr",
        madLevel = 4
      )

      # Show modal with inputs for outlier detection
      showModal(modalDialog(
        title = "Grouped covariates score distribution - Detailed View",
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l",
        tagList(
          fluidRow(
            column(4,
                   radioButtons(ns("modalOutlierMethod"), "Outlier detection method:",
                                choices = c("IQR (using interquartile range)" = "iqr",
                                            "MAD (using median absolute deviation)" = "mad"),
                                selected = modal_inputs$method)
            ),
            column(4,
                   conditionalPanel(
                     condition = sprintf("input.%s == 'mad'", ns("modalOutlierMethod")),
                     numericInput(ns("modalMadLevel"), "MAD multiplier (Median +/- multiplier * MAD):",
                                  value = modal_inputs$madLevel, min = 2, max = 10, step = 0.5)
                   )
            ),
            column(4,
                   checkboxInput(ns("modalShowOutliers"), "Show all data (including outliers)", value = TRUE)
            )
          ),
          tabsetPanel(
            tabPanel("Histogram",
                     plotly::plotlyOutput(ns("modalHistPlot"), height = "400px")
            ),
            tabPanel("Boxplot",
                     plotly::plotlyOutput(ns("modalBoxPlot"), height = "400px")
            )
          )
        )
      ))

      # Update reactive values when user changes inputs
      observe({
        modal_inputs$method <- input$modalOutlierMethod
        if (!is.null(input$modalMadLevel)) modal_inputs$madLevel <- input$modalMadLevel
      })

      # Reactive expression for outlier calculation
      outlier_data <- reactive({
        if (modal_inputs$method == "mad") {
          med <- median(values_expanded)
          mad_val <- mad(values_expanded, constant = 1)
          lowerBound <- med - modal_inputs$madLevel * mad_val
          upperBound <- med + modal_inputs$madLevel * mad_val
        } else {
          Q1 <- quantile(values_expanded, 0.25)
          Q3 <- quantile(values_expanded, 0.75)
          IQR_val <- Q3 - Q1
          lowerBound <- Q1 - 1.5 * IQR_val
          upperBound <- Q3 + 1.5 * IQR_val
        }

        dist_data$isOutlier <- ifelse(dist_data$value < lowerBound | dist_data$value > upperBound,
                                      "outlier_group_value", "")

        # Filter if user wants only non-outliers
        if (!input$modalShowOutliers) {
          dist_data <- dist_data[dist_data$isOutlier != "outlier_group_value", ]
        }

        dist_data
      })


      # Histogram
      output$modalHistPlot <- plotly::renderPlotly({
        df <- outlier_data()
        plotly::plot_ly(
          df,
          x = ~value,
          y = ~n,
          type = 'bar',
          marker = list(color = ifelse(df$isOutlier == "outlier_group_value", "#E74C3C", "#3498DB"))
        ) |>
          plotly::layout(
            xaxis = list(title = "Group score", dtick = 1, tickmode = "linear"),
            yaxis = list(title = "Frequency"),
            showlegend = FALSE
          )
      })

      # Boxplot
      output$modalBoxPlot <- plotly::renderPlotly({
        df <- outlier_data()
        df_expanded <- do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
          data.frame(value = rep(df$value[i], df$n[i]), isOutlier = rep(df$isOutlier[i], df$n[i]))
        }))
        df_expanded$outlier_flag <- ifelse(df_expanded$isOutlier == "outlier_group_value", "Outlier", "Normal")
        p <- ggplot2::ggplot(df_expanded, ggplot2::aes(x = "", y = value)) +
          ggplot2::geom_boxplot(outlier.shape = NA) +
          ggplot2::geom_jitter(aes(color = outlier_flag), width = 0.2, size = 2) +
          ggplot2::labs(x = "Group score", y = "score distribution") +
          ggplot2::theme_minimal() +
          ggplot2::scale_color_manual(values = c("Normal" = "black", "Outlier" = "#E74C3C"))
        plotly::ggplotly(p)
      })
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

    output$downloadGroupedCovariates <- shiny::downloadHandler(
      filename = function() {
        fmt <- input$export_trigger
        ext <- if (fmt %in% c("tsv_long", "tsv_wide")) "tsv" else fmt
        paste0("Grouped_covariates_fullData.", ext)
      },
      content = function(file) {
        req(input$export_trigger %in% c("json", "tsv_wide", "tsv_long"))
        df <- r_groupedCovariates$groupedCovariatesTibble

        if (input$export_trigger == "json") {
          jsonlite::write_json(df, file, pretty = TRUE, auto_unbox = TRUE)

        } else if (input$export_trigger == "tsv_wide") {
          # collapse list columns into comma-separated strings
          df_wide <- df |>
            dplyr::mutate(
              dplyr::across(where(is.list),
                            ~ vapply(.x, paste, collapse = ", ", character(1)))
            )
          readr::write_tsv(df_wide, file)

        } else if (input$export_trigger == "tsv_long") {
          # one row per covariate per group
          df_long <- df |>
            # drop distribution; it's group-level, not per covariate
            dplyr::select(
              groupId,
              groupName,
              covariateIds,
              conceptCodes,
              covariateNames
            ) |>
            # unnest list-columns
            tidyr::unnest(c(covariateIds, conceptCodes, covariateNames)) |>
            dplyr::rename(
              covariateId   = covariateIds,
              conceptCode   = conceptCodes,
              covariateName = covariateNames
            )

          readr::write_tsv(df_long, file)
        }
      }
    )

    shinyjs::disable("downloadGroupedCovariates")

    observeEvent(input$export_trigger, {
      if (input$export_trigger %in% c("json", "tsv_wide", "tsv_long")) {
        shinyjs::enable("downloadGroupedCovariates")
      } else {
        shinyjs::disable("downloadGroupedCovariates")
      }
    })

    .parse_covariates_distribution <- function(x) {
      if (is.na(x) || x == "") {
        return(tibble::tibble(value = numeric(), n = integer()))
      }

      # Expected strings are like this from the tsv wide:
      # "c(3, 4, 2, 5, 1), c(352, 300, 283, 256, 191)"
      parts <- strsplit(x, "\\),\\s*c\\(")[[1]]
      parts <- gsub("^c\\(", "", parts)
      parts <- gsub("\\)$", "", parts)

      # Safely parse both numeric vectors
      vec_value <- suppressWarnings(as.numeric(strsplit(parts[1], ",\\s*")[[1]]))
      vec_n     <- suppressWarnings(as.integer(strsplit(parts[2], ",\\s*")[[1]]))

      tibble::tibble(
        value = vec_value,
        n = vec_n
      )
    }


    observeEvent(input$uploadGroupedCovariates, {
      req(input$uploadGroupedCovariates)

      file <- input$uploadGroupedCovariates$datapath
      ext <- tools::file_ext(input$uploadGroupedCovariates$name)

      if (ext == "json") {
        df <- jsonlite::read_json(file, simplifyVector = TRUE)
        df <- tibble::as_tibble(df)

      } else if (ext == "tsv") {
        df_tsv <- readr::read_tsv(file, show_col_types = FALSE)

        required_cols <- c("groupId", "groupName", "covariateIds", "conceptCodes", "covariateNames")
        if (!all(required_cols %in% names(df_tsv))) {
          shiny::showNotification("Invalid TSV format. Expected wide format tsv file.", type = "error")
          return(NULL)
        }

        df <- df_tsv |>
          dplyr::mutate(
            # split list-columns by comma
            covariateIds   = purrr::map(covariateIds,   ~ as.numeric(strsplit(.x, ",\\s*")[[1]])),
            conceptCodes   = purrr::map(conceptCodes,   ~ strsplit(.x, ",\\s*")[[1]]),
            covariateNames = purrr::map(covariateNames, ~ strsplit(.x, ",\\s*")[[1]]),
            # parse the special tibble-like column
            covariatesDistribution = purrr::map(covariatesDistribution, .parse_covariates_distribution)
          )

      } else {
        shiny::showNotification("Unsupported file format. Please upload .json or .tsv.", type = "error")
        return(NULL)
      }

      # update reactive datasets
      for(codeGrp in 1:nrow(df)){

        df_codeGroup <- df[codeGrp,]
        res <- .appendCovariateGroup(
          analysisResults = analysisResults,
          covariateIds = unlist(df_codeGroup$covariateIds),
          newGroupName = df_codeGroup$groupName,
          groupedCovariatesTibble = r_groupedCovariates$groupedCovariatesTibble,
          groupedCovariatesPerPersonTibble = r_groupedCovariates$groupedCovariatesPerPersonTibble
        )

        r_groupedCovariates$groupedCovariatesTibble <- res$groupedCovariatesTibble
        r_groupedCovariates$groupedCovariatesPerPersonTibble <- res$groupedCovariatesPerPersonTibble

      }

      shiny::showNotification("Code groups imported successfully !! ", type = "message")
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
    rf_totalScoreFormula_res <- mod_fct_dragAndDropFormula_server(
      id = "totalScoreFormula_formula",
      r_groupedCovariates = r_groupedCovariates,
      operatorItems = c(
        `(` = "(", `)` = ")",
        `+` = "+",
        `-` = "-",
        `*` = "*",
        `/` = "/"
      ),
      titleText = "Create Total Score Formula:",
      placeholder = "Drag and Drop here to create formula"
    )
    rf_totalScoreFormula = rf_totalScoreFormula_res$get_formula



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
    rf_flagsTable_list <- mod_fct_phenotypeFlags_server("phenotypeFlags_flags", r_groupedCovariates)
    rf_flagsTable <- rf_flagsTable_list[["r_flagstable"]]
    rf_flagsTableOrder <- rf_flagsTable_list[["r_roworder"]]

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

      # reorder flagsTable rows if rows are sorted by user
      if(!is.null(rf_flagsTableOrder())){
        flagsTable <- flagsTable[rf_flagsTableOrder(), , drop = FALSE]
      }

      # Apply flags
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


      min_score <- floor(min(groupedCovariatesPerPersonTibble$totalScore, na.rm = TRUE))
      max_score <- ceiling(max(groupedCovariatesPerPersonTibble$totalScore, na.rm = TRUE))

      shiny::updateSliderInput(
        session,
        "scoreRange",
        min = min_score,
        max = max_score,
        value = c(min_score, max_score),
        step = 1
      )
      rv_scoreRanges$defaultRange <- c(min_score, max_score)
    })

    #
    # When r_groupedCovariates is ready or slider is changed, plot the total score distribution, if flag is available, add flag to the plot
    #
    output$totalScoreDistributionPlot <- plotly::renderPlotly({
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble |> nrow() > 0)
      shiny::req(input$scoreRange)
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)

      groupedCovariatesPerPersonTibble <- r_groupedCovariates$groupedCovariatesPerPersonTibble |>
        dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore, by = "personSourceValue") |>
        dplyr::mutate(flag = r_groupedCovariates$groupedCovariatesPerPersonTibble_flag$flag[match(personSourceValue, r_groupedCovariates$groupedCovariatesPerPersonTibble_flag$personSourceValue)] %||% "no-flag")

      flagsTable <- rf_flagsTable() |>
        dplyr::bind_rows(tibble::tibble(flagName = "no-flag", flagColor = "grey"))

      selected_bins <- r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore |>
        dplyr::filter(totalScore >= input$scoreRange[1], totalScore <= input$scoreRange[2]) |>
        dplyr::pull(totalScoreBin) |>
        unique()

      groupedCovariatesPerPersonTibble <- groupedCovariatesPerPersonTibble |>
        dplyr::mutate(is_selected = totalScoreBin %in% selected_bins,
                      is_selected_label = dplyr::if_else(is_selected, "In selected Range", "Not In Range")
                      )

      p <- ggplot2::ggplot(groupedCovariatesPerPersonTibble, ggplot2::aes(x = totalScoreBin)) +
        ggplot2::geom_bar(aes(fill = flag, alpha = is_selected_label), position = "stack") +
        ggplot2::scale_alpha_manual(values = c("In selected Range" = 1, "Not In Range" = 0.2), guide = FALSE) +
        ggplot2::scale_fill_manual(values = setNames(flagsTable$flagColor, flagsTable$flagName)) +
        #ggplot2::scale_alpha_identity(guide = "none") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Total Score", y = "Number of Patients") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      plotly::ggplotly(p) |> plotly::layout(legend = list(title = list(text = "Flag and score")))
    })

    #
    # When r_groupedCovariates is ready or slider is changed, plot the total score distribution using density plot, if flag is available, add flag to the plot
    #

    output$totalScoreDensityPlot <- plotly::renderPlotly({
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)
      shiny::req(input$scoreRange)

      df <- r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore |>
        dplyr::filter(totalScore >= input$scoreRange[1], totalScore <= input$scoreRange[2]) |>
        dplyr::mutate(flag = r_groupedCovariates$groupedCovariatesPerPersonTibble_flag$flag[match(personSourceValue, r_groupedCovariates$groupedCovariatesPerPersonTibble_flag$personSourceValue)] %||% "no-flag")

      flagsTable <- rf_flagsTable() |>
        dplyr::bind_rows(tibble::tibble(flagName = "no-flag", flagColor = "grey"))

      p <- ggplot2::ggplot(df, ggplot2::aes(x = totalScore, fill = flag)) +
        ggplot2::geom_density(alpha = 0.6) +
        ggplot2::scale_fill_manual(values = setNames(flagsTable$flagColor, flagsTable$flagName)) +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })


    #
    # When r_groupedCovariates is ready or slider is changed, plot an upset plot for the groups, if flag data is selected, also for the flag data
    #

    output$upsetPlot <- shiny::renderPlot({

      # testing version. For now no selection by score
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble)
      groupedCovariatesPerPersonTibble <- r_groupedCovariates$groupedCovariatesPerPersonTibble
      shiny::req(nrow(groupedCovariatesPerPersonTibble) > 0)

      # Require at least two groups
      if (ncol(groupedCovariatesPerPersonTibble) <= 2) {
        shiny::showNotification("There are not enough code groups to generate an UpSet plot.", type = "warning")
        return(NULL)
      }

      upset_list <- list()
      for(grp in colnames(groupedCovariatesPerPersonTibble)){
        grpWithValues = groupedCovariatesPerPersonTibble$personSourceValue[groupedCovariatesPerPersonTibble[,grp] > 0 ]
        upset_list[[grp]] <- grpWithValues
      }

      # Require at least two groups
      if (sum(lapply(upset_list, length) > 0) <= 2) {
        shiny::showNotification("There are not enough code groups to generate an UpSet plot.", type = "warning")
        return(NULL)
      }


      UpSetR::upset(
        UpSetR::fromList(upset_list[!names(upset_list) %in% "personSourceValue"]),
        order.by = "freq",
        sets.bar.color = "gray20",
        main.bar.color = "black",
        keep.order = TRUE,
        set_size.show = FALSE,
        set_size.scale_max = NULL,
        point.size = 5,
        line.size = 1.8,
        mb.ratio = c(0.7, 0.3),
        text.scale = c(2.5,2.0,2.5,2.0,2.5,3.5),
        sets.x.label = "Code group size"
      )
    })

    #
    # When r_groupedCovariates is ready or slider is changed, show table of total scores
    #

    output$totalScoreTable <- DT::renderDataTable({
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)
      shiny::req(input$scoreRange)

      groupedCovariatesPerPers <- r_groupedCovariates$groupedCovariatesPerPersonTibble

      colnames(groupedCovariatesPerPers)[-1] <- r_groupedCovariates$groupedCovariatesTibble$groupName[
        match(colnames(groupedCovariatesPerPers)[-1],
              r_groupedCovariates$groupedCovariatesTibble$groupId)]

      df <- groupedCovariatesPerPers |>
        dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore, by = "personSourceValue") |>
        dplyr::filter(totalScore >= input$scoreRange[1], totalScore <= input$scoreRange[2])


      # Add flag information if it exists
      if (!is.null(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag) &&
          nrow(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag) > 0) {
        df <- df |>
          dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag, by = "personSourceValue") |>
          dplyr::rename(Flag = flag)
      }

      DT::datatable(df)
    })

    #
    # When r$groupOfCovariatesObject is ready or slider is changed, update the selected patients count
    #
    output$selectedPatientsCount <- shiny::renderUI({
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble |> nrow() > 0, input$scoreRange)
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)

      groupedCovariatesPerPersonTibble <- r_groupedCovariates$groupedCovariatesPerPersonTibble |>
        dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore, by = "personSourceValue")


      # Count subjects in the selected range
      nSelected <- groupedCovariatesPerPersonTibble |>
        dplyr::filter(totalScore >= input$scoreRange[1], totalScore <= input$scoreRange[2]) |>
        nrow()

     if(nrow(rf_flagsTable()) > 0){

       groupedCovariatesPerPersonTibble <-  groupedCovariatesPerPersonTibble |>
         dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag, by = "personSourceValue")

       df_selected <- groupedCovariatesPerPersonTibble |>
         dplyr::filter(totalScore >= input$scoreRange[1], totalScore <= input$scoreRange[2])

       # Counts by flag
       countsByFlag <- df_selected |>
         dplyr::group_by(flag) |>
         dplyr::summarise(n = dplyr::n(), .groups = "drop")


       flag_text <- if(nrow(countsByFlag) > 0){
         paste0(apply(countsByFlag, 1, function(x) paste0(x["flag"], ": ", x["n"])), collapse = "; ")
       } else {
         ""
       }
       dispText = paste0("Number of patients selected: ", nSelected, "<br>", "[ Counts by flag: ",flag_text," ]","<br>")

     }else{

       dispText = paste0("Number of patients selected: ", nSelected)
     }

      htmltools::HTML(dispText)
    })

    #
    # Populate the download choices with the added flags
    #
    shiny::observe({
      shiny::req(rf_flagsTable())
      if(nrow(rf_flagsTable()) > 0){
       flag_choices <- c("All Data","no-flag",unique(rf_flagsTable()$flagName))
      }else{
      flag_choices <- c("All Data",unique(rf_flagsTable()$flagName))
        }
      updateSelectInput(
        inputId = "downloadFlagSelection",
        choices = flag_choices,
        selected = "All Data"
      )
    })

    #
    # When input$exportSelectedSubjects is clicked, export all or flagged subjects with total score in the range of the slider (if check box ticked).
    #
    output$exportSelectedSubjects <- shiny::downloadHandler(
      filename = function() {

        flag <- input$downloadFlagSelection
        userRange <- !identical(as.numeric(input$scoreRange), as.numeric(rv_scoreRanges$defaultRange))

        range_label <- if(userRange==T) {
          paste0("in_range_", input$scoreRange[1], "_to_", input$scoreRange[2])
        } else {
          "full_score_range"
        }

        flag_label <- if (!is.null(flag) && flag != "All Data") {
          gsub("\\s+", "_", tolower(flag))
        } else {
          "all_data"
        }

        paste0("subjects_withflag_", flag_label, "_", range_label, ".tsv")
      },
      content = function(file) {
        shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble |> nrow() > 0)
        shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)

        groupedCovariatesPerPers <- r_groupedCovariates$groupedCovariatesPerPersonTibble

        colnames(groupedCovariatesPerPers)[-1] <- r_groupedCovariates$groupedCovariatesTibble$groupName[
                    match(colnames(groupedCovariatesPerPers)[-1],
                          r_groupedCovariates$groupedCovariatesTibble$groupId)]

        df <- groupedCovariatesPerPers |>
          dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore, by = "personSourceValue")

        if (is.null(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag)) {
          df <- df |> dplyr::mutate(flag = "no-flag")
        } else {
          df <- df |> dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag, by = "personSourceValue")
        }

        # filter by selected flag (unless "All Data")
        if (!is.null(input$downloadFlagSelection) && input$downloadFlagSelection != "All Data") {
          df <- df |> dplyr::filter(flag == input$downloadFlagSelection)
        }

        # filter by the score range
        df <- df |> dplyr::filter(
            totalScore |> as.integer() >= input$scoreRange[1],
            totalScore |> as.integer() <= input$scoreRange[2]
          )


        write.table(df, file, sep = "\t", row.names = FALSE, na = "", quote = FALSE)
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
  analysisResults |>
    dplyr::tbl("codewasResults") |>
    dplyr::left_join(analysisResults |> dplyr::tbl("covariateRef"), by = c("covariateId" = "covariateId")) |>
    dplyr::left_join(analysisResults |> dplyr::tbl("analysisRef"), by = c("analysisId" = "analysisId")) |>
    # TEMP
    dplyr::filter(!(vocabularyId == "ATC" & nchar(conceptCode) < 7)) |>
    # END TEMP
    dplyr::left_join(
      analysisResults |> dplyr::tbl("covariatesPerPerson") |>
        dplyr::distinct(covariateId) |>
        dplyr::mutate(isDataAvailable = 1),
      by = c("covariateId" = "covariateId")
    ) |>
    dplyr::collect() |>
    dplyr::mutate(isDataAvailable = ifelse(is.na(isDataAvailable), FALSE, TRUE))
}


#' Append Covariate Group
#' @description Appends a new group of covariates to the groupOfCovariatesObject
#' @param analysisResults A database connection containing analysis results tables
#' @param covariateIds A vector of covariate ids
#' @param groupedCovariatesTibble A tibble containing the grouped covariates
#' @param newGroupName Character string giving the name for the new covariate group.
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

   # Parse formula to catch syntax errors
  parsed_formula <- tryCatch(
    parse(text = formula),
    error = function(e) stop(paste("Formula syntax error:", e$message))
  )

  # Calculate total scores
  groupedCovariatesPerPersonTibble_totalScore <- groupedCovariatesPerPersonTibble |>
    dplyr::mutate(totalScore = eval(parsed_formula))

  score_values <- groupedCovariatesPerPersonTibble_totalScore$totalScore
  unique_vals <- length(unique(score_values))

  # Default to percentile breaks
  percentile_breaks <- unique(quantile(score_values, probs = seq(0, 1, by = 0.05), na.rm = TRUE))

  # Choose breaks adaptively
  if (unique_vals < 5) {
    breaks <- sort(unique(score_values))
  } else if (length(percentile_breaks) >= 5) {
    breaks <- percentile_breaks
  } else {
    breaks <- pretty(score_values, n = min(10, unique_vals))
  }

  # Ensure breaks fully cover range
  if (length(breaks) == 1) breaks <- c(breaks - 0.5, breaks + 0.5)
  if (min(breaks) > min(score_values, na.rm = TRUE)) breaks <- c(min(score_values, na.rm = TRUE), breaks)
  if (max(breaks) < max(score_values, na.rm = TRUE)) breaks <- c(breaks, max(score_values, na.rm = TRUE))

  # Bin scores
  groupedCovariatesPerPersonTibble_totalScore <- groupedCovariatesPerPersonTibble_totalScore |>
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

  values_expanded <- rep(covariatesDistribution$value, covariatesDistribution$n)

  # Calculate IQR to detect outliers on the score values
  Q1 <- quantile(values_expanded, 0.25, na.rm = TRUE)
  Q3 <- quantile(values_expanded, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lowerBound <- Q1 - 1.5 * IQR
  upperBound <- Q3 + 1.5 * IQR

  covariatesDistribution$isOutlier <- with(covariatesDistribution, ifelse(value < lowerBound | value > upperBound,"outlier_group_value",""))

  plot <- plotly::plot_ly(
    data = covariatesDistribution,
    x = ~as.factor(value),
    y = ~n,
    type = "bar",
    marker = list(color = ~ifelse(isOutlier == "outlier_group_value", "#E74C3C", "#3498DB"))
  ) |>
    plotly::layout(
      height = 150,
      width = 250,
      xaxis = list(title = "Group score"),
      yaxis = list(title = "Freq"),
      showlegend = FALSE
    ) |>
    plotly::config(displayModeBar = FALSE)


  htmltools::tags$div( title = "Click for larger view",
                       style = "width: 250px; height: 150px; cursor: pointer;",
                       plot)
}


