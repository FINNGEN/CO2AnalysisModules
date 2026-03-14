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
            shiny::h4("Code Groups Table"),
            shiny::checkboxInput(
              ns("showAdvancedDistributions"),
              "Show advanced distributions (days/age)",
              value = FALSE
            )
          ),
          shiny::column(
            width = 4,
            align = "right",
            shiny::div(
              shiny::fileInput(
                ns("uploadGroupedCovariates"),
                label = "Import Code Groups",
                accept = c(".json"),
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
                  "JSON (for use in lifetrack)" = "json",
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

      # =========================
      # Builders row: Formula + Flags
      # =========================
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::wellPanel(
            style = "padding: 15px; background-color: #f8f9fa;",
            shiny::h4("Total score formula"),
            shiny::tags$p(
              style = "margin-top: -6px; color: #6c757d;",
              "Drag items into the dashed area to build the score formula."
            ),
            mod_fct_dragAndDropFormula_ui(ns("totalScoreFormula_formula")),
            shiny::tags$div(style = "margin-top: 10px;"),
            shiny::tags$strong("Formula message:"),
            shiny::verbatimTextOutput(ns("totalScoreFormula_text"), placeholder = TRUE)
          )
        ),

        shiny::column(
          width = 6,
          shiny::wellPanel(
            style = "padding: 15px; background-color: #f8f9fa;",
            shiny::h4("Flags"),
            shiny::tags$p(
              style = "margin-top: -6px; color: #6c757d;",
              "Create labels based on selected groups or score ranges."
            ),
            mod_fct_phenotypeFlags_ui(ns("phenotypeFlags_flags")),
            shiny::tags$div(style = "margin-top: 10px;"),
            shiny::verbatimTextOutput(ns("phenotypeFlags_text"), placeholder = TRUE)
          )
        )
      ),

      shiny::hr(),

      # =========================
      # Results row: Plots + Actions
      # =========================
      shiny::fluidRow(
        shiny::column(
          width = 8,
          tabsetPanel(
            id = ns("scorePlotTabs"),
            tabPanel("Total Score Bar Plot",
                     plotly::plotlyOutput(ns("totalScoreDistributionPlot"), height = "450px")),
            tabPanel("Density Plot",
                     plotly::plotlyOutput(ns("totalScoreDensityPlot"), height = "450px")),
            tabPanel("Upset Plot",
                     shiny::div(
                       upsetjs::upsetjsOutput(ns("upsetPlot"), height = "450px", width = "auto"),
                       shiny::uiOutput(ns("upsetFlagButtonUI"))
                     )),
            tabPanel("Score Table",
                     DT::dataTableOutput(ns("totalScoreTable")))
          ),

          shiny::br(),
          shiny::uiOutput(ns("selectedPatientsCount")),

          shiny::sliderInput(
            ns("scoreRange"),
            "Score Range",
            width = "100%",
            min = 0,
            max = 10,  # updated dynamically
            value = c(0, 10),
            step = 1,
            ticks = TRUE
          )
        ),

        shiny::column(
          width = 4,
          shiny::wellPanel(
            style = "padding: 15px; background-color: #f8f9fa;",
            shiny::h4("Download"),
            shiny::selectInput(
              ns("downloadFlagSelection"),
              "Selection for download:",
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
      conceptIds = list(),
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

    # track selection in upset plot
    r_upset_selection <- shiny::reactiveValues(
      sets = NULL,
      name = NULL,
      cardinality = NULL
    )

    r_sets_list <- shiny::reactiveValues(sets_list=NULL, set_ids=NULL)


    .counts_only_per_person <- shiny::reactive({
      req(r_groupedCovariates$groupedCovariatesPerPersonTibble)
      df <- r_groupedCovariates$groupedCovariatesPerPersonTibble
      keep <- c("personSourceValue", .group_count_cols(df))
      df[, keep, drop = FALSE]
    })



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
    .makeDistCol <- function(metric, colname, width = 300) {
      reactable::colDef(
        name = colname,
        width = width,
        cell = function(value, index) {
          plotDiv <- .renderCovariatesDistribution(value, metric = metric)

          # send BOTH index + metric so modal knows what to show
          payload <- jsonlite::toJSON(list(index = index, metric = metric), auto_unbox = TRUE)

          htmltools::tags$div(
            onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", ns("showDistPlot"), payload),
            style = "cursor:pointer;",
            plotDiv
          )
        },
        html = TRUE
      )
    }

    output$groupedCovariatesTable <- reactable::renderReactable({

      toPlot <- r_groupedCovariates$groupedCovariatesTibble |>
        dplyr::select(-conceptIds)


      # IMPORTANT: if advanced columns are shown, they must exist in the data
      if (isTRUE(input$showAdvancedDistributions)) {
        toPlot <- toPlot |>
          dplyr::mutate(
            dist_daysToFirst = covariatesDistribution,
            dist_daysToLast  = covariatesDistribution,
            dist_ageFirst    = covariatesDistribution
          )
      }

      toPlot <- toPlot |> dplyr::mutate(editButton = NA, deleteButton = NA)

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
            as.character(htmltools::tags$div(
              title = full, HTML(display),
              style = "transition: background-color 0.3s ease;",
              onmouseover = "this.style.backgroundColor='#ffffcc'; this.style.cursor='pointer';",
              onmouseout = "this.style.backgroundColor='';"
            ))
          },
          html = TRUE
        ),

        covariateNames = reactable::colDef(
          name = "Covariate Names",
          minWidth = 100,
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
            as.character(htmltools::tags$div(
              title = full, HTML(display),
              style = "transition: background-color 0.3s ease;",
              onmouseover = "this.style.backgroundColor='#ffffcc'; this.style.cursor='pointer';",
              onmouseout = "this.style.backgroundColor='';"
            ))
          },
          html = TRUE
        ),

        covariatesDistribution = .makeDistCol("count", "Counts (# Events)", width = 300),

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

      # Now these column defs will work, because the columns exist in toPlot
      if (isTRUE(input$showAdvancedDistributions)) {
        insert_after <- match("covariatesDistribution", names(columns))

        columns <- append(
          columns,
          list(
            dist_daysToFirst = .makeDistCol("daysToFirst", "Days to First Event", width = 300),
            dist_daysToLast  = .makeDistCol("daysToLast",  "Days to Last Event",  width = 300),
            dist_ageFirst    = .makeDistCol("ageFirst",    "Age First Event",     width = 300)
          ),
          after = insert_after
        )
      }

      reactable::reactable(
        toPlot,
        columns = columns,
        resizable = TRUE
      )
    })


    observeEvent(input$showDistPlot, {

      click <- input$showDistPlot

      # click can be either:
      # - old style: a single numeric index
      # - new style: a list with $index and $metric
      if (is.list(click)) {
        idx <- click$index
        metric <- click$metric %||% "count"
      } else {
        idx <- click
        metric <- "count"
      }

      dist_data <- r_groupedCovariates$groupedCovariatesTibble$covariatesDistribution[[idx]]

      # pick the correct metric distribution
      if (is.list(dist_data) && !is.data.frame(dist_data)) {
        dist_data <- dist_data[[metric]] %||% dist_data$count
      }

      if (is.null(dist_data) || nrow(dist_data) == 0) return(NULL)

      x_title <- .metric_x_title(metric)
      is_days_metric <- metric %in% c("daysToFirst", "daysToLast")

      # order binned days properly (same as mini plot)
      if (is_days_metric) {
        dist_data <- .order_binned_dist(dist_data)
      }

      # expand values ONLY if numeric; binned strings can't be expanded to numeric safely
      values_expanded <- NULL
      if (all(!is.na(suppressWarnings(as.numeric(dist_data$value))))) {
        values_expanded <- rep(as.numeric(dist_data$value), dist_data$n)
      }

      # --- raw per-person values for boxplot/outliers ---
      group_id <- r_groupedCovariates$groupedCovariatesTibble$groupId[[idx]]

      metric_col <- switch(
        metric,
        count       = group_id,
        daysToFirst = paste0(group_id, "_daysToFirst"),
        daysToLast  = paste0(group_id, "_daysToLast"),
        ageFirst    = paste0(group_id, "_ageFirst"),
        group_id
      )

      raw_values <- NULL
      df_wide_raw <- r_groupedCovariates$groupedCovariatesPerPersonTibble

      if (!is.null(df_wide_raw) && metric_col %in% names(df_wide_raw)) {
        raw_values <- df_wide_raw[[metric_col]]
        raw_values <- suppressWarnings(as.numeric(raw_values))
        raw_values <- raw_values[!is.na(raw_values)]
      }


      modal_inputs <- reactiveValues(
        method = "iqr",
        madLevel = 4
      )

      # Show modal with inputs for outlier detection
      showModal(modalDialog(
        title = paste0("Distribution: ", x_title),
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

        df <- dist_data

        # If we do not have numeric expanded values (e.g., binned "(0,10]"), skip outlier detection
        if (is.null(values_expanded) || length(values_expanded) == 0) {
          df$isOutlier <- ""
          return(df)
        }

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

        # value may be numeric or character; compare safely
        val_num <- suppressWarnings(as.numeric(df$value))
        df$isOutlier <- ifelse(!is.na(val_num) & (val_num < lowerBound | val_num > upperBound),
                               "outlier_group_value", "")

        if (!input$modalShowOutliers) {
          df <- df[df$isOutlier != "outlier_group_value", ]
        }

        df
      })

      # outlier values for raw data
      outlier_values <- reactive({
        v <- raw_values
        if (is.null(v) || length(v) == 0) return(list(values = numeric(), is_outlier = logical()))

        if (modal_inputs$method == "mad") {
          med <- median(v)
          mad_val <- mad(v, constant = 1)
          lowerBound <- med - modal_inputs$madLevel * mad_val
          upperBound <- med + modal_inputs$madLevel * mad_val
        } else {
          Q1 <- quantile(v, 0.25)
          Q3 <- quantile(v, 0.75)
          IQR_val <- Q3 - Q1
          lowerBound <- Q1 - 1.5 * IQR_val
          upperBound <- Q3 + 1.5 * IQR_val
        }

        is_out <- (v < lowerBound | v > upperBound)

        if (!isTRUE(input$modalShowOutliers)) {
          v <- v[!is_out]
          is_out <- is_out[!is_out]  # becomes all FALSE
        }

        list(values = v, is_outlier = is_out)
      })

      # Histogram
      output$modalHistPlot <- plotly::renderPlotly({
        df <- outlier_data()

        p <- plotly::plot_ly(
          df,
          x = ~value,
          y = ~n,
          type = 'bar',
          marker = list(color = ifelse(df$isOutlier == "outlier_group_value", "#E74C3C", "#3498DB"))
        ) |>
          plotly::layout(
            xaxis = list(title = x_title),
            yaxis = list(title = "Frequency"),
            showlegend = FALSE
          )

        # enforce correct ordering for binned days
        if (is_days_metric && is.factor(df$value)) {
          p <- p |> plotly::layout(
            xaxis = list(
              title = x_title,
              type = "category",
              categoryorder = "array",
              categoryarray = levels(df$value)
            )
          )
        }

        p
      })

      # Boxplot
      output$modalBoxPlot <- plotly::renderPlotly({

        ov <- outlier_values()
        vals <- ov$values
        if (length(vals) == 0) return(NULL)

        df_pts <- data.frame(
          value = vals,
          outlier_flag = ifelse(ov$is_outlier, "Outlier", "Normal"),
          x = 0
        )

        # jitter x manually so points spread horizontally
        set.seed(1)
        df_pts$xj <- df_pts$x + stats::runif(nrow(df_pts), -0.15, 0.15)

        p <- plotly::plot_ly() |>
          plotly::add_trace(
            type = "box",
            y = vals,
            x = rep(0, length(vals)),
            name = "",
            showlegend = FALSE,
            boxpoints = FALSE,
            hovertemplate = paste0(
              "Value: %{y}<extra></extra>"
            )
          ) |>
          # Points colored by outlier flag
          plotly::add_markers(
            data = df_pts,
            x = ~xj,
            y = ~value,
            marker = list(
              size = 7,
              color = ifelse(df_pts$outlier_flag == "Outlier",
                             "#E74C3C",
                             "#3498DB")
            ),
            text = ~outlier_flag,
            hovertemplate = paste0(
              "%{text}<br>",
              "Value: %{y}<extra></extra>"
            ),
            showlegend = FALSE
          ) |>
          plotly::layout(
            xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(title = x_title),
            legend = list(title = list(text = "outlier_flag"))
          )

        # add legends
        p <- p |>
          plotly::add_trace(
            x = NA, y = NA,
            type = "scatter",
            mode = "markers",
            marker = list(size = 7, color = "#3498DB"),
            name = "Normal"
          ) |>
          plotly::add_trace(
            x = NA, y = NA,
            type = "scatter",
            mode = "markers",
            marker = list(size = 7, color = "#E74C3C"),
            name = "Outlier"
          )

        p
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
              covariatesDistribution = vapply(
                covariatesDistribution,
                function(x) jsonlite::toJSON(x, auto_unbox = TRUE),
                character(1)
              ),
              dplyr::across(
                .cols = where(is.list) & !dplyr::all_of("covariatesDistribution"),
                ~ vapply(.x, paste, collapse = ", ", character(1))
              )
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
              conceptIds,
              conceptCodes,
              covariateNames
            ) |>
            # unnest list-columns
            tidyr::unnest(c(covariateIds, conceptIds, conceptCodes, covariateNames))

          readr::write_tsv(df_long, file)
        }
      }
    )

    shinyjs::disable("downloadGroupedCovariates")

    # observeEvent(input$export_trigger, {
    #   if (input$export_trigger %in% c("json", "tsv_wide", "tsv_long")) {
    #     shinyjs::enable("downloadGroupedCovariates")
    #   } else {
    #     shinyjs::disable("downloadGroupedCovariates")
    #   }
    # })

    observeEvent(input$export_trigger, {
      if (input$export_trigger %in% c("json", "tsv_wide", "tsv_long")) {
        shinyjs::enable("downloadGroupedCovariates")
      } else {
        shinyjs::disable("downloadGroupedCovariates")
        # prevent download if disabled
        shinyjs::runjs(
          sprintf(
            "$('#%s').off('click').on('click', function(e) {
            if ($(this).prop('disabled')) {
              e.preventDefault();
              e.stopImmediatePropagation();
            }
          });",
            ns("downloadGroupedCovariates")
          )
        )
      }
    })

    .parse_covariates_distribution <- function(x) {
      if (is.na(x) || x == "") return(list())

      # If it's JSON (new way)
      if (grepl("^\\s*\\{", x) || grepl("^\\s*\\[", x)) {
        return(jsonlite::fromJSON(x, simplifyVector = TRUE))
      }

      # Legacy wide string fallback: "c(...), c(...)"
      parts <- strsplit(x, "\\),\\s*c\\(")[[1]]
      parts <- gsub("^c\\(", "", parts)
      parts <- gsub("\\)$", "", parts)

      vec_value <- suppressWarnings(as.numeric(strsplit(parts[1], ",\\s*")[[1]]))
      vec_n     <- suppressWarnings(as.integer(strsplit(parts[2], ",\\s*")[[1]]))

      tibble::tibble(value = vec_value, n = vec_n)
    }

    # helpers that can parse numeric OR character OR empty safely
    .parse_csv_num <- function(x) {
      if (is.list(x)) x <- unlist(x, use.names = FALSE)
      if (length(x) == 0 || all(is.na(x))) return(numeric(0))

      x <- as.character(x)[1]
      x <- trimws(x)
      if (!nzchar(x)) return(numeric(0))

      parts <- unlist(strsplit(x, ",\\s*"))
      suppressWarnings(as.numeric(parts))
    }

    .parse_csv_chr <- function(x) {
      if (is.list(x)) x <- unlist(x, use.names = FALSE)
      if (length(x) == 0 || all(is.na(x))) return(character(0))

      x <- as.character(x)[1]
      x <- trimws(x)
      if (!nzchar(x)) return(character(0))

      unlist(strsplit(x, ",\\s*"))
    }


    observeEvent(input$uploadGroupedCovariates, {
      req(input$uploadGroupedCovariates)

      file <- input$uploadGroupedCovariates$datapath
      ext <- tools::file_ext(input$uploadGroupedCovariates$name)

      # for now import only json files. tsv formatted files export only
      if (tolower(ext) == "json") {
        df <- jsonlite::read_json(file, simplifyVector = TRUE)
        df <- tibble::as_tibble(df)

      # } else if (ext == "tsv") {
      #   df_tsv <- readr::read_tsv(file, show_col_types = FALSE)
      #
      #
      #   required_cols <- c("groupId", "groupName", "covariateIds", "conceptIds","conceptCodes", "covariateNames")
      #   if (!all(required_cols %in% names(df_tsv))) {
      #     shiny::showNotification("Invalid TSV format. Expected wide format tsv file.", type = "error")
      #     return(NULL)
      #   }
      #
      #   df <- df_tsv |>
      #     dplyr::mutate(
      #       covariateIds = purrr::map(.data$covariateIds, .parse_csv_num),
      #       conceptIds   = purrr::map(.data$conceptIds,   .parse_csv_num),
      #       conceptCodes = purrr::map(.data$conceptCodes, .parse_csv_chr),
      #       covariateNames = purrr::map(.data$covariateNames, .parse_csv_chr),
      #       covariatesDistribution = purrr::map(.data$covariatesDistribution, .parse_covariates_distribution)
      #     )
      #
      } else {
        shiny::showNotification("Unsupported file format. Please upload .json formatted file", type = "error")
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
    # prepare items for formula when groupedCovariatesTibble is ready
    #

    r_formula_items <- shiny::reactive({
      shiny::req(r_groupedCovariates$groupedCovariatesTibble)

      groups <- r_groupedCovariates$groupedCovariatesTibble
      ids <- groups$groupId
      names <- groups$groupName

      # base items (counts)
      items <- stats::setNames(ids, names)  # names=labels, values=tokens


      if (!is.null(r_groupedCovariates$groupedCovariatesPerPersonTibble)) {
        df <- r_groupedCovariates$groupedCovariatesPerPersonTibble

        add_if_exists <- function(token, label) {
          if (token %in% names(df)) {
            items <<- c(items, stats::setNames(token, label))
          }
        }

        for (i in seq_along(ids)) {
          add_if_exists(paste0(ids[i], "_daysToFirst"), paste0(names[i], " (daysToFirst)"))
          add_if_exists(paste0(ids[i], "_daysToLast"),  paste0(names[i], " (daysToLast)"))
          add_if_exists(paste0(ids[i], "_ageFirst"),    paste0(names[i], " (ageFirst)"))
        }
      }

      items
    })

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
      placeholder = "Drag and Drop here to create formula",
      variableItems = r_formula_items
    )
    rf_totalScoreFormula = rf_totalScoreFormula_res$get_formula



    #
    # when input$formula is changed, attempt to use it
    # if it is valid, calculate totalScore and totalScoreBin columns
    # if it is invalid, show error message and delete totalScore and totalScoreBin columns
    #

    # determine the unit of the variable (token)
    .get_token_unit <- function(token) {
      if (grepl("^g\\d+_daysTo(First|Last)$", token)) return("days")
      if (grepl("^g\\d+_ageFirst$", token)) return("years")
      if (grepl("^g\\d+$", token)) return("count")
      "unknown"
    }

    # helper: combine units for * and /
    .combine_units_muldiv <- function(u1, u2, op) {
      # unitless handling
      if (u1 == "unitless") return(u2)
      if (u2 == "unitless") return(u1)

      if (op == "*") return(paste0(u1, "*", u2))
      if (op == "/") return(paste0(u1, "/", u2))
      "unknown"
    }

    # infer unit recursively + validate operator constraints
    .infer_unit <- function(expr) {
      # numeric literal
      if (is.numeric(expr)) return(list(ok = TRUE, unit = "unitless"))

      # symbol (variable like g3, g3_daysToFirst, ...)
      if (is.symbol(expr)) {
        tok <- as.character(expr)
        u <- .get_token_unit(tok)
        if (u == "unknown") {
          return(list(ok = FALSE, message = paste0("Unknown variable: ", tok)))
        }
        return(list(ok = TRUE, unit = u))
      }

      # call (operator expression)
      if (is.call(expr)) {
        op <- as.character(expr[[1]])

        # parentheses for grouping
        if (op %in% c("(") && length(expr) == 2) {
          return(.infer_unit(expr[[2]]))
        }

        # unary + / -
        if (op %in% c("+", "-") && length(expr) == 2) {
          res <- .infer_unit(expr[[2]])
          return(res)
        }

        # binary operators
        if (op %in% c("+", "-", "*", "/") && length(expr) == 3) {
          left  <- .infer_unit(expr[[2]])
          if (!left$ok) return(left)
          right <- .infer_unit(expr[[3]])
          if (!right$ok) return(right)

          if (op %in% c("+", "-")) {

            # allow adding/subtracting numeric constants to/from any unit
            if (left$unit == "unitless" && right$unit != "unitless") {
              return(list(ok = TRUE, unit = right$unit))
            }
            if (right$unit == "unitless" && left$unit != "unitless") {
              return(list(ok = TRUE, unit = left$unit))
            }

            # must match units (unitless can only add/sub with unitless)
            if (left$unit != right$unit) {
              return(list(
                ok = FALSE,
                message = paste0(
                  "Invalid formula: cannot use '", op, "' between different units.\n",
                  "Left: ", left$unit, " | Right: ", right$unit
                )
              ))
            }

            return(list(ok = TRUE, unit = left$unit))
          }

          # * or / allowed, returns derived unit
          unit_out <- .combine_units_muldiv(left$unit, right$unit, op)
          return(list(ok = TRUE, unit = unit_out))
        }

        # if there are other functions/operators, fail fast (or extend later)
        return(list(
          ok = FALSE,
          message = paste0(
            "Invalid formula: unsupported operation '", op, "'."
          )
        ))
      }

      list(ok = FALSE, message = "Invalid formula: could not parse expression.")
    }

    .validate_score_formula_units <- function(formula) {
      parsed <- tryCatch(parse(text = formula)[[1]], error = function(e) NULL)
      if (is.null(parsed)) {
        return(list(ok = FALSE, message = "Invalid formula: could not parse expression."))
      }

      res <- .infer_unit(parsed)
      if (!isTRUE(res$ok)) return(list(ok = FALSE, message = res$message))

      # optional: show resulting unit somewhere if you want
      list(ok = TRUE, message = NULL, unit = res$unit)
    }


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

      # check if unit is mixed
      unit_check <- .validate_score_formula_units(formula)
      if (!isTRUE(unit_check$ok)) {
        r$errorMessageTotalScore <- unit_check$message
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
    rf_flagsTable_list <- mod_fct_phenotypeFlags_server("phenotypeFlags_flags", r_groupedCovariates,r_formula_items)
    rf_flagsTable <- rf_flagsTable_list[["r_flagstable"]]
    rf_flagsTableOrder <- rf_flagsTable_list[["r_roworder"]]

    #
    # Evaluate the flag formulas
    #
    shiny::observe({
      shiny::req(rf_flagsTable())
      #shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)

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


    # helper: choose a "nice" step based on the score range
    .nice_step <- function(rng) {
      rng <- as.numeric(rng)
      if (!is.finite(rng) || rng <= 0) return(1)

      # target about 100 steps across the slider
      raw <- rng / 100

      # nice step = 1, 0.5, 0.2, 0.1 * 10^k
      pow10 <- 10^floor(log10(raw))
      m <- raw / pow10

      nice_mult <- if (m <= 1) 1 else if (m <= 2) 2 else if (m <= 5) 5 else 10
      nice_mult * pow10
    }

    # helper: round bounds to multiples of step
    .snap_bounds <- function(minv, maxv, step) {
      minv2 <- floor(minv / step) * step
      maxv2 <- ceiling(maxv / step) * step
      if (!is.finite(minv2) || !is.finite(maxv2) || minv2 == maxv2) {
        # fallback: widen a bit
        minv2 <- minv - step
        maxv2 <- maxv + step
      }
      c(minv2, maxv2)
    }


    shiny::observe({
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble |> nrow() > 0)
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)

      groupedCovariatesPerPersonTibble <- r_groupedCovariates$groupedCovariatesPerPersonTibble |>
        dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore, by = "personSourceValue")

      scores <- groupedCovariatesPerPersonTibble$totalScore
      scores <- scores[is.finite(scores)]
      shiny::req(length(scores) > 0)

      min_score <- min(scores, na.rm = TRUE)
      max_score <- max(scores, na.rm = TRUE)

      rng <- max_score - min_score
      step <- .nice_step(rng)
      bounds <- .snap_bounds(min_score, max_score, step)

      shiny::updateSliderInput(
        session,
        "scoreRange",
        min   = bounds[1],
        max   = bounds[2],
        value = bounds,
        step  = step
      )
      rv_scoreRanges$defaultRange <- bounds
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
    # When r_groupedCovariates is ready or slider is changed, plot an interactive upset plot for the groups, if intersection bar is select, have the option to add it to flag table
    #

    output$upsetPlot <- upsetjs::renderUpsetjs({

      # testing version. For now no selection by score
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble)

      groupedCovariatesPerPersonTibble <- .counts_only_per_person()
      setids <- .group_count_cols(groupedCovariatesPerPersonTibble)

      shiny::req(nrow(groupedCovariatesPerPersonTibble) > 0)



      # use the group names
      # Use group names for display, but keep ids for formulas
      display_names <- r_groupedCovariates$groupedCovariatesTibble$groupName[
        match(setids, r_groupedCovariates$groupedCovariatesTibble$groupId)
      ]
      colnames(groupedCovariatesPerPersonTibble)[match(setids, names(groupedCovariatesPerPersonTibble))] <- display_names

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


      sets_list <- upset_list[!names(upset_list) %in% "personSourceValue"]

      # UpSetR::upset(
      #   UpSetR::fromList(sets_list),
      #   nsets = length(sets_list),
      #   order.by = "freq",
      #   sets.bar.color = "gray20",
      #   main.bar.color = "black",
      #   keep.order = TRUE,
      #   set_size.show = FALSE,
      #   set_size.scale_max = NULL,
      #   point.size = 5,
      #   line.size = 1.8,
      #   mb.ratio = c(0.7, 0.3),
      #   text.scale = c(2.5,2.0,2.5,2.0,2.5,3.5),
      #   sets.x.label = "Code group size"
      # )

      r_sets_list$sets_list <- sets_list
      r_sets_list$set_ids <- setids

      upsetjs::upsetjs() |>
        upsetjs::fromList(sets_list) |>
        upsetjs::generateDistinctIntersections() |>
        upsetjs::chartLabels(set.name = "Code group size") |>
        upsetjs::chartStyleFlags(export.buttons = F)

    })

    output$upsetFlagButtonUI <- shiny::renderUI({
      if (!is.null(r_upset_selection$sets) && length(r_upset_selection$sets) > 0) {
        shiny::actionButton(
          ns("addUpsetIntersectionFlag"),
          paste("Add", r_upset_selection$name, "as Flag"),
          class = "btn-primary",
          style = "margin-top: 10px;"
        )
      } else if(is.null(r_upset_selection$sets) && !is.null(r_upset_selection$name)){
        shiny::actionButton(
          ns("addUpsetIntersectionFlag"),
          paste("Add", r_upset_selection$name, "as Flag"),
          class = "btn-primary",
          style = "margin-top: 10px;"
        )
      }
    })

    # track clicks of intersection bars in upset plot
    observeEvent(input$upsetPlot_click, {
      click_data <- input$upsetPlot_click

      if (!is.null(click_data$name) && click_data$name != "") {
        # Extract intersection info from click_data
        intersection_name <- click_data$name
        set_names <- unlist(click_data$setNames)
        elements <- unlist(click_data$elems)
        cardinality <- click_data$cardinality


        # Store clicked intersection information
        r_upset_selection$sets <- set_names
        r_upset_selection$name <- intersection_name
        r_upset_selection$cardinality <- cardinality

        # Show notification
        shiny::showNotification(
          sprintf("Selected intersection: %s (%d members)",
                  intersection_name, cardinality),
          type = "message",
          duration = 3
        )
      }
    })

    # adding of flag from upset intersection
    observeEvent(input$addUpsetIntersectionFlag, {
      if (!is.null(r_upset_selection$sets) && length(r_upset_selection$sets) > 0) {

        all_groups <- names(r_sets_list$sets_list)
        all_group_ids <- r_sets_list$set_ids

        sets_in <- r_upset_selection$sets
        sets_not_in <- setdiff(all_groups, sets_in)

        sets_in_ids = all_group_ids[match(sets_in,all_groups)]
        sets_not_in_ids = all_group_ids[match(sets_not_in,all_groups)]

        # Create formula that allows intersections as flag e.g (set1 > 0) & (set2 == 0) & (set3 == 0) ...
        positive_parts <- paste(sets_in_ids, "> 0")
        zero_parts <- paste(sets_not_in_ids, "== 0")

        flag_formula <- paste(c(positive_parts, zero_parts), collapse = " & ")

        # Create descriptive name
        if (length(sets_in) == 1) {
          flag_name <- paste("Only", sets_in)
        } else {
          flag_name <- paste("Intersection:", paste(sets_in, collapse = " & "))
        }

      } else if(is.null(r_upset_selection$sets) && !is.null(r_upset_selection$name)){
        # Create a formula that allows adding single sets as flag (set1 > 0, other sets may be present or not)
        all_groups <- names(r_sets_list$sets_list)
        all_group_ids <- r_sets_list$set_ids

        idForGroup = all_group_ids[match(r_upset_selection$name,all_groups)]

        flag_formula <- paste(idForGroup, "> 0", collapse = " ")
        flag_name <- paste("Code group:", paste(r_upset_selection$name, collapse = " "))
      }

        # Store in session for the flags module
        session$userData$upset_flag_data <- list(
          name = flag_name,
          formula = flag_formula
        )

        # Trigger the add flag button in flags module
        shinyjs::click("phenotypeFlags_flags-addFlag_button")

        # Clear selection after adding
        r_upset_selection$sets <- NULL
        r_upset_selection$name <- NULL

    })


    #
    # When r_groupedCovariates is ready or slider is changed, show table of total scores
    #

    output$totalScoreTable <- DT::renderDataTable({
      shiny::req(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)
      shiny::req(input$scoreRange)

      groupedCovariatesPerPers <- r_groupedCovariates$groupedCovariatesPerPersonTibble

      df <- groupedCovariatesPerPers |>
        dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore, by = "personSourceValue") |>
        dplyr::filter(is.infinite(totalScore) | (totalScore >= input$scoreRange[1] & totalScore <= input$scoreRange[2]))

      df <- .rename_group_cols_to_names(df, r_groupedCovariates$groupedCovariatesTibble)


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

        if(!is.null(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore)){

          groupedCovariatesPerPers <- r_groupedCovariates$groupedCovariatesPerPersonTibble

          df <- groupedCovariatesPerPers |>
            dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_totalScore, by = "personSourceValue")

          df <- .rename_group_cols_to_names(df, r_groupedCovariates$groupedCovariatesTibble)


          if (is.null(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag)) {
            df <- df |> dplyr::mutate(flag = "no-flag")
          } else {
            df <- df |> dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag, by = "personSourceValue")
          }

          # filter by selected flag (unless "All Data")
          if (!is.null(input$downloadFlagSelection) && input$downloadFlagSelection != "All Data") {
            df <- df |> dplyr::filter(flag == input$downloadFlagSelection)
          }

          # filter by the score range (also keep inf values)
          # No possibility for a -Inf, but this should be improved if such a situation exists
          df <- df |> dplyr::filter(is.infinite(totalScore) | (totalScore >= input$scoreRange[1] & totalScore <= input$scoreRange[2]))

        }else{
          # enable downloading of flag data when formula for total score is not set
          groupedCovariatesPerPers <- r_groupedCovariates$groupedCovariatesPerPersonTibble

          colnames(groupedCovariatesPerPers)[-1] <- r_groupedCovariates$groupedCovariatesTibble$groupName[
            match(colnames(groupedCovariatesPerPers)[-1],
                  r_groupedCovariates$groupedCovariatesTibble$groupId)]

          df <- groupedCovariatesPerPers |>
            dplyr::left_join(r_groupedCovariates$groupedCovariatesPerPersonTibble_flag, by = "personSourceValue")


          if (!is.null(input$downloadFlagSelection) && input$downloadFlagSelection != "All Data") {
            df <- df |> dplyr::filter(flag == input$downloadFlagSelection)
          }
        }

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
# .getcodeWasCovariatesTibble <- function(analysisResults) {
#   analysisResults |>
#     dplyr::tbl("codewasResults") |>
#     dplyr::left_join(analysisResults |> dplyr::tbl("covariateRef"), by = c("covariateId" = "covariateId")) |>
#     dplyr::left_join(analysisResults |> dplyr::tbl("analysisRef"), by = c("analysisId" = "analysisId")) |>
#     # TEMP
#     dplyr::filter(!(vocabularyId == "ATC" & nchar(conceptCode) < 7)) |>
#     # END TEMP
#     dplyr::left_join(
#       analysisResults |> dplyr::tbl("covariatesPerPerson") |>
#         dplyr::distinct(covariateId) |>
#         dplyr::mutate(isDataAvailable = 1),
#       by = c("covariateId" = "covariateId")
#     ) |>
#     dplyr::collect() |>
#     dplyr::mutate(isDataAvailable = ifelse(is.na(isDataAvailable), FALSE, TRUE))
# }

#' Get CodeWAS covariates tibble for Phenotype Scoring
#'
#' @description
#' Builds the CodeWAS results table for binary feature analysis used to discover and group codes for phenotype
#' scoring.
#'
#' @param analysisResults An Andromeda object (DuckDB-backed) containing the FeatureExtraction
#'   result tables: `analysisRef`, `conceptRef`, `covariates`, `cohortCounts`,
#'   `personLevelData`, and `statisticalTests`.
#'
#' @return A tibble of binary association results including concept metadata, case/control
#'   counts (`nCasesYes`, `nControlsYes`), prevalence (`meanCases`, `meanControls`), and
#'   `isDataAvailable`. For legacy compatibility it also includes `covariateId` (=`conceptId`)
#'   and `covariateName` (=`conceptName`).
#'
#' @importFrom dplyr tbl left_join inner_join filter select distinct transmute mutate rename collect
.getcodeWasCovariatesTibble <- function(analysisResults) {


  binTestOnly = tryCatch({
    DBI::dbExistsTable(analysisResults, "codewasResults")
  }, error = function(e) FALSE)

  # if codewas run was based on binary features only

  if(binTestOnly){

    covTibToRet <- analysisResults |>
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

  }else{

    analysisRef  <- analysisResults |> dplyr::tbl("analysisRef")
    conceptRef   <- analysisResults |> dplyr::tbl("conceptRef")
    covariates   <- analysisResults |> dplyr::tbl("covariates")
    cohortCounts <- analysisResults |> dplyr::tbl("cohortCounts")
    personLevel  <- analysisResults |> dplyr::tbl("personLevelData")

    # Binary association results + refs
    base <- analysisResults |>
      dplyr::tbl("statisticalTests") |>
      dplyr::rename(oddsRatio = effectSize) |>
      dplyr::left_join(analysisRef, by = "analysisId") |>
      dplyr::filter(analysisType == "Binary") |>
      dplyr::left_join(conceptRef, by = "conceptId") |>

      # TEMP: remove short ATC codes (this is consistent with the previous approach, needs to be changed if needed)
      dplyr::filter(!(vocabularyId == "ATC" & nchar(conceptCode) < 7))

    # Case/control nYes counts from covariates
    keys <- base |>
      dplyr::distinct(analysisId, caseCohortId, controlCohortId, conceptId)

    cov_bin <- covariates |>
      dplyr::select(analysisId, cohortDefinitionId, conceptId, sumValue)

    counts <- keys |>
      # CASES
      dplyr::left_join(
        cov_bin |>
          dplyr::transmute(
            analysisId,
            caseCohortId = cohortDefinitionId,
            conceptId,
            nCasesYes = sumValue
          ),
        by = c("analysisId", "caseCohortId", "conceptId")
      ) |>
      # CONTROLS
      dplyr::left_join(
        cov_bin |>
          dplyr::transmute(
            analysisId,
            controlCohortId = cohortDefinitionId,
            conceptId,
            nControlsYes = sumValue
          ),
        by = c("analysisId", "controlCohortId", "conceptId")
      ) |>
      dplyr::mutate(
        nCasesYes = dplyr::if_else(is.na(nCasesYes), 0, nCasesYes),
        nControlsYes = dplyr::if_else(is.na(nControlsYes), 0, nControlsYes)
      ) |>
      # summary information
      dplyr::left_join(cohortCounts, by = c("caseCohortId" = "cohortId")) |>
      dplyr::rename(nCasesTotal = cohortSubjects) |>
      dplyr::left_join(cohortCounts, by = c("controlCohortId" = "cohortId")) |>
      dplyr::rename(nControlsTotal = cohortSubjects) |>
      dplyr::mutate(
        meanCases = dplyr::if_else(nCasesTotal > 0, nCasesYes / nCasesTotal, NA_real_),
        sdCases   = sqrt(meanCases * (1 - meanCases)),
        meanControls = dplyr::if_else(nControlsTotal > 0, nControlsYes / nControlsTotal, NA_real_),
        sdControls   = sqrt(meanControls * (1 - meanControls))
      ) |>
      dplyr::select(
        analysisId, caseCohortId, controlCohortId, conceptId,
        nCasesYes, nControlsYes, meanCases, sdCases, meanControls, sdControls
      )

    # Availability at person-level for the save binary analysisId + conceptId (covariate ids)
    # this answers if the concept id is detected in the cases cohort at all
    # but since we only need to check the availability of conceptid for the persons regardless of the analysis id at this stage,
    # it is enough to just check for conceptid availibility
    available <- personLevel |>
      dplyr::select(conceptId) |>
      dplyr::distinct() |>
      dplyr::mutate(isDataAvailable = 1)

    # Final table
    covTibToRet <- base |>
      dplyr::left_join(
        counts,
        by = c("analysisId", "caseCohortId", "controlCohortId", "conceptId")
      ) |>
      dplyr::left_join(
        available,
        by = c("conceptId")
      ) |>
      dplyr::mutate(
        isDataAvailable = dplyr::if_else(is.na(isDataAvailable), FALSE, TRUE),
        covariateId = conceptId,
        covariateName = conceptName
      ) |>
      dplyr::collect()

  }


  return(covTibToRet)

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
# .appendCovariateGroup <- function(
#     analysisResults,
#     covariateIds,newGroupName,
#     groupedCovariatesTibble,
#     groupedCovariatesPerPersonTibble) {
#
#   if (nrow(groupedCovariatesTibble) == 0) {
#     newGroupId <- 1
#   } else {
#     existingIds <- groupedCovariatesTibble$groupId
#     existingNums <- as.integer(sub("g", "", existingIds))
#     newGroupId <- max(existingNums, na.rm = TRUE) + 1
#   }
#
#   sumAllCovariatesPerPerson <- analysisResults |>
#     dplyr::tbl("covariatesPerPerson") |>
#     dplyr::distinct(personSourceValue) |>
#     dplyr::left_join(
#       analysisResults |>
#         dplyr::tbl("covariatesPerPerson") |>
#         dplyr::filter(covariateId %in% covariateIds) |>
#         dplyr::group_by(personSourceValue) |>
#         dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop"),
#       by = "personSourceValue"
#     ) |>
#     dplyr::mutate(value = ifelse(is.na(value), 0, value)) |>
#     dplyr::collect()
#
#   covariatesDistribution <- sumAllCovariatesPerPerson |>
#     dplyr::count(value, sort = TRUE)
#
#   # create a new group
#   conceptCodes <- analysisResults |>
#     dplyr::tbl("covariateRef") |>
#     dplyr::filter(covariateId %in% covariateIds) |>
#     dplyr::pull(conceptCode)
#
#   # bring the concept ids (omop ids)
#   conceptIds <- analysisResults |>
#     dplyr::tbl("covariateRef") |>
#     dplyr::filter(covariateId %in% covariateIds) |>
#     dplyr::pull(conceptId)
#
#
#   covariateNames <- analysisResults |>
#     dplyr::tbl("covariateRef") |>
#     dplyr::filter(covariateId %in% covariateIds) |>
#     dplyr::pull(covariateName)
#
#   groupTibble <- tibble::tibble(
#     groupId = paste0("g", newGroupId),
#     groupName = newGroupName,
#     covariateIds = list(covariateIds),
#     conceptIds = list(conceptIds),
#     conceptCodes = list(conceptCodes),
#     covariateNames = list(covariateNames),
#     covariatesDistribution = list(covariatesDistribution)
#   )
#
#   sumAllCovariatesPerPerson <- sumAllCovariatesPerPerson |>
#     dplyr::rename(!!paste0("g", newGroupId) := value)
#
#   # append
#   groupedCovariatesTibble <- dplyr::bind_rows(groupedCovariatesTibble, groupTibble)
#   if (is.null(groupedCovariatesPerPersonTibble)) {
#     groupedCovariatesPerPersonTibble <- sumAllCovariatesPerPerson
#   } else {
#     groupedCovariatesPerPersonTibble <- dplyr::left_join(groupedCovariatesPerPersonTibble, sumAllCovariatesPerPerson, by = "personSourceValue")
#   }
#
#   return(list(
#     groupedCovariatesTibble = groupedCovariatesTibble,
#     groupedCovariatesPerPersonTibble = groupedCovariatesPerPersonTibble
#   ))
# }


#' Append Code Group (Phenotype Scoring)
#'
#' @description
#' Creates a new code group from selected `conceptId`s and appends it to:
#' 1) `groupedCovariatesTibble` (group metadata + score distribution)
#' 2) `groupedCovariatesPerPersonTibble` (one numeric column per group)
#'
#' @param analysisResults An Andromeda (DuckDB-backed) object containing at least:
#'   `analysisRef`, `conceptRef`, `comparisons`, and `personLevelData`.
#' @param covariateIds Integer vector of OMOP conceptIds selected into the group.
#'   (For legacy naming, these are the "covariateIds".)
#' @param newGroupName Character name for the new group.
#' @param groupedCovariatesTibble Tibble of existing groups (can be empty).
#' @param groupedCovariatesPerPersonTibble Tibble with `personSourceValue` and
#'   one column per existing group (can be NULL).
#'
#' @return A list with:
#'   - `groupedCovariatesTibble`
#'   - `groupedCovariatesPerPersonTibble`
#'
#' @importFrom dplyr tbl filter select distinct mutate summarise group_by left_join rename count
#' @importFrom tibble tibble
#' @importFrom rlang .data
.appendCovariateGroup <- function(
    analysisResults,
    covariateIds,
    newGroupName,
    groupedCovariatesTibble,
    groupedCovariatesPerPersonTibble
) {

  binTestOnly = tryCatch({
    DBI::dbExistsTable(analysisResults, "codewasResults")
  }, error = function(e) FALSE)

  if (binTestOnly) {

    # --- KEEP AS IS FOR NOW (legacy binary-only results) ---
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

    conceptCodes <- analysisResults |>
      dplyr::tbl("covariateRef") |>
      dplyr::filter(covariateId %in% covariateIds) |>
      dplyr::pull(conceptCode)

    conceptIds <- analysisResults |>
      dplyr::tbl("covariateRef") |>
      dplyr::filter(covariateId %in% covariateIds) |>
      dplyr::pull(conceptId)

    covariateNames <- analysisResults |>
      dplyr::tbl("covariateRef") |>
      dplyr::filter(covariateId %in% covariateIds) |>
      dplyr::pull(covariateName)

    groupTibble <- tibble::tibble(
      groupId = paste0("g", newGroupId),
      groupName = newGroupName,
      covariateIds = list(covariateIds),
      conceptIds = list(conceptIds),
      conceptCodes = list(conceptCodes),
      covariateNames = list(covariateNames),
      covariatesDistribution = list(covariatesDistribution)
    )

    sumAllCovariatesPerPerson <- sumAllCovariatesPerPerson |>
      dplyr::rename(!!paste0("g", newGroupId) := value)

    groupedCovariatesTibble <- dplyr::bind_rows(groupedCovariatesTibble, groupTibble)
    if (is.null(groupedCovariatesPerPersonTibble)) {
      groupedCovariatesPerPersonTibble <- sumAllCovariatesPerPerson
    } else {
      groupedCovariatesPerPersonTibble <- dplyr::left_join(
        groupedCovariatesPerPersonTibble,
        sumAllCovariatesPerPerson,
        by = "personSourceValue"
      )
    }

    return(list(
      groupedCovariatesTibble = groupedCovariatesTibble,
      groupedCovariatesPerPersonTibble = groupedCovariatesPerPersonTibble
    ))

  } else {

    # --- UPDATED BRANCH (DuckDB/Andromeda FeatureExtraction tables) ---

    # determine new group id
    if (nrow(groupedCovariatesTibble) == 0) {
      newGroupNum <- 1L
    } else {
      existingNums <- suppressWarnings(as.integer(sub("^g", "", groupedCovariatesTibble$groupId)))
      newGroupNum <- max(existingNums, na.rm = TRUE) + 1L
    }
    newGroupId <- paste0("g", newGroupNum)

    analysisRef     <- analysisResults |> dplyr::tbl("analysisRef")
    conceptRef      <- analysisResults |> dplyr::tbl("conceptRef")
    comparisons     <- analysisResults |> dplyr::tbl("comparisons")
    personLevelData <- analysisResults |> dplyr::tbl("personLevelData")

    conceptIds <- covariateIds

    caseCohortId <- comparisons |>
      dplyr::select(caseCohortId) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(caseCohortId)

    caseCohortId <- caseCohortId[1]


    # pick the analysis ids by analysisType
    countsAnalysisIds <- analysisRef |>
      dplyr::filter(.data$analysisType == "Counts") |>
      dplyr::select(.data$analysisId) |>
      dplyr::distinct()

    daysFirstAnalysisIds <- analysisRef |>
      dplyr::filter(.data$analysisType == "DaysToFirstEvent") |>
      dplyr::select(.data$analysisId) |>
      dplyr::distinct()

    daysLastAnalysisIds <- analysisRef |>
      dplyr::filter(.data$analysisType == "DaysToLastEvent") |>
      dplyr::select(.data$analysisId) |>
      dplyr::distinct()

    ageFirstAnalysisIds <- analysisRef |>
      dplyr::filter(.data$analysisType == "AgeFirstEvent") |>
      dplyr::select(.data$analysisId) |>
      dplyr::distinct()

    # all persons in CASE cohort
    allPersons <- personLevelData |>
      dplyr::filter(.data$cohortDefinitionId == !!caseCohortId) |>
      dplyr::select(.data$personSourceValue) |>
      dplyr::distinct()

    .bin_days_human <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return(tibble::tibble(value = character(), n = integer()))

      # Create broader bins based on orders of magnitude
      abs_max <- max(abs(x))

      if (abs_max > 10000) {
        # 1000-day bins for very large values
        bin_size <- 1000
      } else if (abs_max > 5000) {
        # 500-day bins
        bin_size <- 500
      } else if (abs_max > 1000) {
        # 100-day bins
        bin_size <- 100
      } else {
        # 30-day bins for smaller ranges
        bin_size <- 30
      }

      # Create breaks
      min_break <- floor(min(x) / bin_size) * bin_size
      max_break <- ceiling(max(x) / bin_size) * bin_size
      inner_breaks <- seq(min_break, max_break, by = bin_size)

      # Add infinities for edges
      breaks <- c(-Inf, inner_breaks, Inf)

      # Create labels: one for each interval (length(breaks) - 1)
      # Use the upper bound of each interval as the label
      labels <- inner_breaks  # for intervals: (-Inf, first], (first, second], ..., (last, Inf]

      # Need to add a label for the last interval (last inner_break to Inf)
      labels <- c(labels, inner_breaks[length(inner_breaks)] + bin_size)

      # Ensure labels length matches breaks length - 1
      if (length(labels) > length(breaks) - 1) {
        labels <- labels[1:(length(breaks)-1)]
      }

      # Use right = TRUE for proper interval closure (a, b] which works better with negatives
      tibble::tibble(bin = cut(x, breaks = breaks, labels = labels,
                               right = TRUE, include.lowest = TRUE)) |>
        dplyr::count(.data$bin, name = "n", sort = FALSE) |>
        dplyr::transmute(value = as.character(.data$bin), n = .data$n)
    }


    .bin_age_human <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return(tibble::tibble(value = character(), n = integer()))

      # Age bins (years): 0-9, 10-17, 18-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80
      breaks <- c(-Inf, 0, 9, 17, 29, 39, 49, 59, 69, 79, Inf)
      labels <- c(
        "<=0",
        "0-9",
        "10-17",
        "18-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "80+"
      )

      tibble::tibble(bin = cut(x, breaks = breaks, labels = labels, right = TRUE, include.lowest = TRUE)) |>
        dplyr::count(.data$bin, name = "n", sort = FALSE) |>
        dplyr::transmute(value = as.character(.data$bin), n = .data$n)
    }


    # Event COUNTS per person
    countsByPerson <- personLevelData |>
      dplyr::filter(.data$cohortDefinitionId == !!caseCohortId) |>
      dplyr::inner_join(countsAnalysisIds, by = "analysisId") |>
      dplyr::filter(.data$conceptId %in% !!conceptIds) |>
      dplyr::group_by(.data$personSourceValue) |>
      dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop")

    countsByPerson <- allPersons |>
      dplyr::left_join(countsByPerson, by = "personSourceValue") |>
      dplyr::mutate(value = dplyr::if_else(is.na(.data$value), 0, .data$value)) |>
      dplyr::collect()

    dist_counts <- countsByPerson |>
      dplyr::count(.data$value, sort = TRUE)

    # DaysToFirstEvent: min (among present codes)
    daysFirstByPerson <- personLevelData |>
      dplyr::filter(.data$cohortDefinitionId == !!caseCohortId) |>
      dplyr::inner_join(daysFirstAnalysisIds, by = "analysisId") |>
      dplyr::filter(.data$conceptId %in% !!conceptIds) |>
      dplyr::group_by(.data$personSourceValue) |>
      dplyr::summarise(
        value = min(.data$value, na.rm = TRUE),
        .groups = "drop"
      )

    daysFirstByPerson <- allPersons |>
      dplyr::left_join(daysFirstByPerson, by = "personSourceValue") |>
      dplyr::collect()

    dist_daysFirst <- .bin_days_human(daysFirstByPerson$value)

    # DaysToLastEvent: max (among present codes)
    daysLastByPerson <- personLevelData |>
      dplyr::filter(.data$cohortDefinitionId == !!caseCohortId) |>
      dplyr::inner_join(daysLastAnalysisIds, by = "analysisId") |>
      dplyr::filter(.data$conceptId %in% !!conceptIds) |>
      dplyr::group_by(.data$personSourceValue) |>
      dplyr::summarise(
        value = max(.data$value, na.rm = TRUE),
        .groups = "drop"
      )

    daysLastByPerson <- allPersons |>
      dplyr::left_join(daysLastByPerson, by = "personSourceValue") |>
      dplyr::collect()

    dist_daysLast <- .bin_days_human(daysLastByPerson$value)

    # AgeFirstEvent: min (among present codes)
    ageFirstByPerson <- personLevelData |>
      dplyr::filter(.data$cohortDefinitionId == !!caseCohortId) |>
      dplyr::inner_join(ageFirstAnalysisIds, by = "analysisId") |>
      dplyr::filter(.data$conceptId %in% !!conceptIds) |>
      dplyr::group_by(.data$personSourceValue) |>
      dplyr::summarise(
        value = min(.data$value, na.rm = TRUE),
        .groups = "drop"
      )

    ageFirstByPerson <- allPersons |>
      dplyr::left_join(ageFirstByPerson, by = "personSourceValue") |>
      dplyr::collect()

    dist_ageFirst <- ageFirstByPerson  |>
      dplyr::count(.data$value, sort = TRUE)


    # per-person wide table for group
    scoreWide <- countsByPerson |>
      dplyr::rename(!!newGroupId := .data$value) |>
      dplyr::left_join(daysFirstByPerson |> dplyr::rename(!!paste0(newGroupId, "_daysToFirst") := .data$value),
                       by = "personSourceValue") |>
      dplyr::left_join(daysLastByPerson  |> dplyr::rename(!!paste0(newGroupId, "_daysToLast") := .data$value),
                       by = "personSourceValue") |>
      dplyr::left_join(ageFirstByPerson  |> dplyr::rename(!!paste0(newGroupId, "_ageFirst") := .data$value),
                       by = "personSourceValue")


    # store all distributions in one list
    covariatesDistribution <- list(
      count = dist_counts,
      daysToFirst = dist_daysFirst,
      daysToLast = dist_daysLast,
      ageFirst = dist_ageFirst
    )

    # group metadata
    conceptMeta <- conceptRef |>
      dplyr::filter(.data$conceptId %in% !!conceptIds) |>
      dplyr::select(.data$conceptId, .data$conceptCode, .data$conceptName) |>
      dplyr::collect()

    groupTibble <- tibble::tibble(
      groupId = newGroupId,
      groupName = newGroupName,
      covariateIds = list(conceptIds),
      conceptIds = list(conceptMeta$conceptId),
      conceptCodes = list(conceptMeta$conceptCode),
      covariateNames = list(conceptMeta$conceptName),
      covariatesDistribution = list(covariatesDistribution)
    )


    groupedCovariatesTibble <- dplyr::bind_rows(groupedCovariatesTibble, groupTibble)

    if (is.null(groupedCovariatesPerPersonTibble)) {
      groupedCovariatesPerPersonTibble <- scoreWide
    } else {
      groupedCovariatesPerPersonTibble <- groupedCovariatesPerPersonTibble |>
        dplyr::left_join(scoreWide, by = "personSourceValue")
    }

    return(list(
      groupedCovariatesTibble = groupedCovariatesTibble,
      groupedCovariatesPerPersonTibble = groupedCovariatesPerPersonTibble
    ))
  }
}


#
#
# .deleteCovariateGroup <- function(
#     rowIndex,
#     groupedCovariatesTibble,
#     groupedCovariatesPerPersonTibble
# ) {
#   # Get the groupId of the row to delete
#   groupIdToDelete <- groupedCovariatesTibble$groupId[rowIndex]
#
#   # Remove the row from groupedCovariatesTibble
#   groupedCovariatesTibble <- groupedCovariatesTibble[-rowIndex, ]
#
#   # Remove the corresponding column from groupedCovariatesPerPersonTibble if exists
#   if (!is.null(groupedCovariatesPerPersonTibble)) {
#     groupedCovariatesPerPersonTibble <- groupedCovariatesPerPersonTibble |>
#       dplyr::select(-dplyr::any_of(groupIdToDelete))
#   }
#
#   return(list(
#     groupedCovariatesTibble = groupedCovariatesTibble,
#     groupedCovariatesPerPersonTibble = groupedCovariatesPerPersonTibble
#   ))
# }

.deleteCovariateGroup <- function(rowIndex, groupedCovariatesTibble, groupedCovariatesPerPersonTibble) {

  groupIdToDelete <- groupedCovariatesTibble$groupId[rowIndex]
  groupedCovariatesTibble <- groupedCovariatesTibble[-rowIndex, , drop = FALSE]

  if (!is.null(groupedCovariatesPerPersonTibble)) {
    cols_to_drop <- c(
      groupIdToDelete,
      paste0(groupIdToDelete, "_daysToFirst"),
      paste0(groupIdToDelete, "_daysToLast"),
      paste0(groupIdToDelete, "_ageFirst")
    )
    groupedCovariatesPerPersonTibble <- groupedCovariatesPerPersonTibble |>
      dplyr::select(-dplyr::any_of(cols_to_drop))
  }

  list(
    groupedCovariatesTibble = groupedCovariatesTibble,
    groupedCovariatesPerPersonTibble = groupedCovariatesPerPersonTibble
  )
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

  # make breaks strictly increasing
  breaks <- sort(unique(breaks))
  if (length(breaks) < 2) {
    b <- breaks[1]
    breaks <- c(b - 0.5, b + 0.5)
  }

  # Bin scores
  groupedCovariatesPerPersonTibble_totalScore <- groupedCovariatesPerPersonTibble_totalScore |>
    dplyr::mutate(totalScoreBin = cut(totalScore, breaks = breaks, include.lowest = TRUE)) |>
    dplyr::select(personSourceValue, totalScore, totalScoreBin)

  return(groupedCovariatesPerPersonTibble_totalScore)
}

#' Render Covariates Distribution
#' @description Renders a plot of the covariates distribution
#' @param covariatesDistribution A tibble containing the covariates distribution
#' @param metric A string indicating the type of data distribution, default is count(number clinical events), and can be AgeFirstEvent, daysToFirstEvent, daysToLastEvent
#' @return A plot of the covariates distribution
#' @importFrom apexcharter apex ax_chart ax_legend
.renderCovariatesDistribution <- function(covariatesDistribution, metric = "count") {

  dist <- covariatesDistribution

  if (is.list(dist) && !is.data.frame(dist)) {
    dist <- dist[[metric]] %||% dist$count %||% dist[[1]]
  }

  if (is.null(dist) || nrow(dist) == 0) return(NULL)
  if (!all(c("value", "n") %in% colnames(dist))) return(NULL)

  x_title <- .metric_x_title(metric)
  is_days_metric <- metric %in% c("daysToFirst", "daysToLast")

  val_num <- suppressWarnings(as.numeric(dist$value))
  has_numeric <- all(!is.na(val_num))

  # outlier detection: keep as you had it (only numeric & not binned-days)
  if (has_numeric && !is_days_metric) {
    values_expanded <- rep(val_num, dist$n)
    Q1 <- quantile(values_expanded, 0.25, na.rm = TRUE)
    Q3 <- quantile(values_expanded, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lowerBound <- Q1 - 1.5 * IQR
    upperBound <- Q3 + 1.5 * IQR
    dist$isOutlier <- ifelse(val_num < lowerBound | val_num > upperBound, "outlier_group_value", "")
    dist <- dist[order(val_num), , drop = FALSE]
  } else {
    dist$isOutlier <- ""
  }

  # order binned days properly
  if (is_days_metric) {
    dist <- .order_binned_dist(dist)
    x_expr <- ~value
    xaxis_cfg <- list(
      title = x_title,
      type = "category",
      categoryorder = "array",
      categoryarray = levels(dist$value)
    )
  } else if (has_numeric) {
    x_expr <- ~as.numeric(value)
    xaxis_cfg <- list(title = x_title)
  } else {
    x_expr <- ~as.factor(value)
    xaxis_cfg <- list(title = x_title)
  }

  plot <- plotly::plot_ly(
    data = dist,
    x = x_expr,
    y = ~n,
    type = "bar",
    marker = list(color = ~ifelse(isOutlier == "outlier_group_value", "#E74C3C", "#3498DB"))
  ) |>
    plotly::layout(
      height = 150,
      width  = 250,
      xaxis  = xaxis_cfg,
      yaxis  = list(title = "Freq"),
      showlegend = FALSE
    ) |>
    plotly::config(displayModeBar = FALSE)

  htmltools::tags$div(
    title = "Click for larger view",
    style = "width: 250px; height: 150px; cursor: pointer;",
    plot
  )
}

# bin ordering related function for day binning ----

.parse_bin_upper <- function(x) {
  # x like "(-1000,0]" or "[0,500]" or "(500,1000]" etc
  # return upper bound numeric, Inf for open-ended
  x <- as.character(x)

  if (grepl("Inf\\]$|Inf\\)$", x)) return(Inf)

  # extract the last number before the closing bracket/paren
  m <- regmatches(x, regexpr("-?\\d+(?:\\.\\d+)?(?=\\s*[\\]\\)])\\s*$", x, perl = TRUE))
  if (length(m) == 0 || is.na(m) || m == "") return(NA_real_)
  suppressWarnings(as.numeric(m))
}

.order_binned_dist <- function(dist) {
  # expects columns: value, n
  key <- vapply(dist$value, .parse_bin_upper, numeric(1))
  ord <- order(key, na.last = TRUE)
  dist <- dist[ord, , drop = FALSE]

  # force factor level order to the sorted order
  dist$value <- factor(dist$value, levels = dist$value, ordered = TRUE)
  dist
}

.metric_x_title <- function(metric) {
  switch(
    metric,
    count       = "Group score",
    daysToFirst = "Days to First (binned)",
    daysToLast  = "Days to Last (binned)",
    ageFirst    = "Age at First (years)",
    "Value"
  )
}



# group name related helper functions ----

.is_group_count_col <- function(x) {
  grepl("^g\\d+$", x)
}

.group_count_cols <- function(df) {
  cols <- names(df)
  cols[.is_group_count_col(cols)]
}

.group_all_cols_for_id <- function(groupId) {
  # includes gX and all gX_* suffix columns
  c(groupId,
    paste0(groupId, "_daysToFirst"),
    paste0(groupId, "_daysToLast"),
    paste0(groupId, "_ageFirst"))
}

.rename_group_count_cols_to_names <- function(df, groups_tbl) {
  # Only rename gX columns; leave suffix columns unchanged
  count_cols <- .group_count_cols(df)
  map <- groups_tbl$groupName[match(count_cols, groups_tbl$groupId)]
  # if any missing, keep original (avoid NA colnames)
  map[is.na(map)] <- count_cols[is.na(map)]
  names(df)[match(count_cols, names(df))] <- map
  df
}

.rename_group_cols_to_names <- function(df, groups_tbl) {
  stopifnot(all(c("groupId", "groupName") %in% names(groups_tbl)))

  cn <- names(df)

  # Columns that are group metrics:
  # gX, gX_daysToFirst, gX_daysToLast, gX_ageFirst
  is_group_col <- grepl("^g\\d+(?:_(?:daysToFirst|daysToLast|ageFirst))?$", cn)
  if (!any(is_group_col)) return(df)

  group_cols <- cn[is_group_col]

  # Extract base id (gX) and suffix (_daysToFirst etc)
  base_id <- sub("_(?:daysToFirst|daysToLast|ageFirst)$", "", group_cols)
  suffix  <- sub("^g\\d+", "", group_cols)

  # Map base id -> groupName
  map <- setNames(groups_tbl$groupName, groups_tbl$groupId)
  new_base <- unname(map[base_id])

  # If missing mapping, keep original base id
  new_base[is.na(new_base) | !nzchar(new_base)] <- base_id[is.na(new_base) | !nzchar(new_base)]

  # Build new names
  new_names <- paste0(new_base, suffix)

  # Avoid duplicates (can happen if two groups share same name)
  new_names <- make.unique(new_names, sep = "__")

  names(df)[match(group_cols, cn)] <- new_names
  df
}

