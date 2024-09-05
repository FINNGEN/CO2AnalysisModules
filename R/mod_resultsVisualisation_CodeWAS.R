#' @title Cohort Demographics Visualization UI
#' @description UI module for visualizing cohort overlaps using an UpSet plot. This module provides controls to customize the appearance of the plot and options to download the plot and data.
#'
#' @param id A string representing the module's namespace.
#'
#' @return A Shiny UI element that can be included in a Shiny app.
#'
#' @importFrom shiny NS fluidPage div fluidRow column actionButton checkboxInput plotOutput downloadButton observeEvent
#' @importFrom shinyWidgets sliderInput chooseSliderSkin
#' @importFrom shinyjs useShinyjs toggle hidden
#' @importFrom htmltools tagList
#' @importFrom shinybrowser detect
#'
#' @export
#'
mod_resultsVisualisation_CodeWAS_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4("Filters"),
    shiny::div(
      style = "margin-top: 10px; margin-bottom: 20px;"
    ),
    shiny::uiOutput(ns("codeWASFilter")),
    htmltools::hr(style = "margin-top: 10px; margin-bottom: 10px;"),
    shiny::tabsetPanel(
      id = ns("tabset"),
      shiny::tabPanel(
        "Plot",
        shiny::div(style = "height: 24px;"),
        ggiraph::girafeOutput(ns("codeWASplot"), width = "80%", height = "500"),
        shiny::div(
          style = "margin-top: 10px; margin-bottom: 10px;",
          shiny::downloadButton(ns("downloadPlot"), "Download")
        )
      ),
      shiny::tabPanel(
        "Table",
        shiny::div(
          style = "margin-top: 10px; margin-bottom: 10px;",
          DT::dataTableOutput(ns("codeWAStable")),
        ),
        shiny::div(
          style = "margin-top: 10px; margin-bottom: 10px;",
          shiny::downloadButton(ns("downloadCodeWAS"), "Download", icon = shiny::icon("download"))
        )
      )
    ), # tabsetPanel
  )
}


#' @title Cohort Demographics Visualization Server
#' @description Server module for handling the logic of the cohort overlaps visualization UI. This module creates an UpSet plot based on the analysis results and allows the plot and data to be downloaded.
#'
#' @param id A string representing the module's namespace.
#' @param analysisResults Pooled connection to the analisys results duckdb.
#'
#' @return The module returns server-side logic to generate and manage the cohort overlaps UpSet plot.
#'
#' @importFrom shiny moduleServer reactive req renderPlot downloadHandler
#' @importFrom shinyjs toggle
#' @importFrom dplyr tbl collect mutate
#' @importFrom UpSetR upset fromExpression
#' @importFrom grDevices cairo_pdf dev.off
#'
#' @export
#'
mod_resultsVisualisation_CodeWAS_server <- function(id, analysisResults) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    atlasUrl <- "TEMP" #shiny::getShinyOption("cohortOperationsConfig")$atlasUrl

    # reactive values
    r <- shiny::reactiveValues(
      codeWASData = NULL,
      filteredCodeWASData = NULL
    )

    #
    # load the CodeWAS data
    #
    shiny::observe({
      r$codeWASData <- analysisResults |> dplyr::tbl('codewasResults') |>
        dplyr::left_join(analysisResults |> dplyr::tbl('covariateRef') , by = c('covariateId' = 'covariateId'))  |>
        dplyr::left_join(analysisResults |> dplyr::tbl('analysisRef') , by = c('analysisId' = 'analysisId')) |>
        dplyr::collect() |>
        dplyr::mutate(oddsRatio = ifelse(is.na(oddsRatio) & modelType != 'linear', exp(beta), oddsRatio)) |>
        dplyr:::select(-c('isBinary', 'missingMeansZero')) |>
        dplyr::mutate(mplog = cut(-log10(pValue),
                                  breaks = c(0, 5, 100, Inf),
                                  labels = c('-log10(p) (0,5]', '-log10(p) (5,100]', '-log10(p) (100,Inf]'))
        )
    })

    #
    # render the CodeWAS filters from the data
    #
    output$codeWASFilter <- shiny::renderUI({
      req(r$codeWASData)

      shiny::fluidRow(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            ns("database"),
            "Database",
            choices = unique(r$codeWASData$databaseId),
            selected = unique(r$codeWASData$databaseId),
            multiple = FALSE,
            options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3", `count-selected-text` = "{0} databases selected")
          )),
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            ns("domain"),
            "Domain",
            choices = unique(r$codeWASData$domainId),
            selected = unique(r$codeWASData$domainId),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = "count > 3",
              `count-selected-text` = "{0} domains selected"
            )
          )),
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            ns("analysis"),
            "Analysis",
            choices = unique(r$codeWASData$analysisName),
            selected = unique(r$codeWASData$analysisName),
            multiple = TRUE,
            options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3", `count-selected-text` = "{0} analyses selected")
          )),
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            ns("model"),
            "Model",
            choices = unique(r$codeWASData$modelType),
            selected = unique(r$codeWASData$modelType),
            multiple = TRUE,
            options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3", `count-selected-text` = "{0} model types selected")
          )),
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            ns("pValue"),
            "p",
            choices = c('-log10(p) (0,5]', '-log10(p) (5,100]', '-log10(p) (100,Inf]'),
            selected = c('-log10(p) (0,5]', '-log10(p) (5,100]', '-log10(p) (100,Inf]'),
            multiple = TRUE,
            options = list(`actions-box` = TRUE, `selected-text-format` = "count > 1", `count-selected-text` = "{0} classes selected")
          )
        )
      )
    })


    #
    # filter the data
    #
    shiny::observe({
      req(r$codeWASData)

      r$filteredCodeWASData <- r$codeWASData |>
        dplyr::filter(
          if (!is.null(input$database)) databaseId %in% input$database else FALSE,
          if (!is.null(input$domain)) domainId %in% input$domain else FALSE,
          if (!is.null(input$analysis)) analysisName %in% input$analysis else FALSE,
          if (!is.null(input$model)) modelType %in% input$model else FALSE,
          if (!is.null(input$pValue)) mplog %in% input$pValue | is.na(mplog) else FALSE
        )
    })

    #
    # render the CodeWAS table
    #
    output$codeWAStable <- DT::renderDataTable({
      req(r$filteredCodeWASData)
      req(r$filteredCodeWASData  |>  nrow() > 0)

      # https://github.com/rstudio/DT/issues/1127
      # the bug can be worked around by setting shiny.json.digits to a smaller value
      options(shiny.json.digits = 4)

      DT::datatable(
        r$filteredCodeWASData |>
          dplyr::mutate(pValue = as.numeric(formatC(pValue, format = "e", digits = 2))) |>
          dplyr::mutate(oddsRatio = as.numeric(formatC(oddsRatio, format = "e", digits = 2))) |>
          dplyr::mutate(beta = as.numeric(formatC(beta, format = "e", digits = 2))) |>
          dplyr::mutate(standardError = as.numeric(formatC(standardError, format = "e", digits = 2))) |>
          dplyr::mutate(meanCases = as.numeric(formatC(meanCases, format = "e", digits = 2))) |>
          dplyr::mutate(sdCases = as.numeric(formatC(sdCases, format = "e", digits = 2))) |>
          dplyr::mutate(meanControls = as.numeric(formatC(meanControls, format = "e", digits = 2))) |>
          dplyr::mutate(sdControls = as.numeric(formatC(sdControls, format = "e", digits = 2))) |>
          dplyr::mutate(covariateNameFull = as.character(covariateName)) |>
          dplyr::mutate(covariateName = stringr::str_trunc(covariateName, 50)) |>
          dplyr::mutate(analysisName = stringr::str_trunc(analysisName, 20)) |>
          dplyr::mutate(
            covariateId = round(covariateId/1000),
            covariateName = purrr::map2_chr(covariateName, covariateId, ~paste0('<a href="',atlasUrl,'/#/concept/', .y, '" target="_blank">', .x,'</a>'))
          ) |>
          dplyr::filter(pValue < 0.0005) |>
          dplyr::select(
            databaseId, domainId, analysisName, covariateName, conceptId,
            nCasesYes, nControlsYes, meanCases, sdCases, meanControls, sdControls,
            pValue, oddsRatio, beta, standardError, modelType, runNotes
          ),
        escape = FALSE,
        class = 'display nowrap compact',
        selection = 'none',
        rownames = FALSE,
        colnames = c(
          'Database' = 'databaseId',
          'Domain' = 'domainId',
          'Analysis' = 'analysisName',
          'Covariate Name' = 'covariateName',
          'Concept ID' = 'conceptId',
          # 'Cov. ID' = 'covariateId',
          # 'N tot' = 'n_total',
          'N case' = 'nCasesYes',
          'N ctrl' = 'nControlsYes',
          'Mean case' = 'meanCases',
          'SD case' = 'sdCases',
          'Mean ctrl' = 'meanControls',
          'SD ctrl' = 'sdControls',
          'p' = 'pValue',
          'OR' = 'oddsRatio',
          'Beta' = 'beta',
          'SE' = 'standardError',
          'Model' = 'modelType',
          # 'ID' = 'analysisId',
          'Notes' = 'runNotes'
          # 'Binary' = 'isBinary',
          # 'Missing mean zero' = 'missingMeansZero'
          # 'covariateNameFull' = 'covariateNameFull'
        ),
        options = list(
            order = list(list(11, 'asc'), list(12, 'desc')), # pValue, OR
          # rowCallback to show the full covariate name as a tooltip
          # rowCallback = htmlwidgets::JS(
          #   "function(row, data) {",
          #   "var full_text = data[13]", # covariateNameFull
          #   "$('td', row).attr('title', full_text);",
          #   "}"
          # ),
          # change the color of the cells with NA (except for the Notes column)
          # createdRow = htmlwidgets::JS(
          #   "function(row, data, dataIndex) {",
          #   "  for(var i=0; i<data.length; i++){",
          #   "    if(data[i] === null && ![5, 6, 12].includes(i)){", # skip Notes-column
          #   "      $('td:eq('+i+')', row).html('NA')",
          #   "        .css({'color': 'rgb(226,44,41)', 'font-style': 'italic'});",
          #   "    }",
          #   "  }",
          #   "}"
          # ),
          # autoWidth = TRUE,
          # arrange the table by pValue
          # scrollX = TRUE,
          columnDefs = list(
            list(width = '70px', targets = c(3, 13)), # covariateName, runNotes
            list(width = '80px', targets = c(2)), # analysisName
            list(width = '60px', targets = c(1)), # domainId
            list(width = '50px', targets = c(4)), # conceptId
            list(width = '40px', targets = c(0,5,6,7, 10, 11)),
            list(width = '50px', targets = c(8, 9)), # pValue, OR
            list(visible = FALSE, targets = c(13)) # covariateNameFull
          ),
          pageLength = 20,
          lengthMenu = c(10, 15, 20, 25, 30)
        )
      ) |> DT::formatStyle('Covariate Name', cursor = 'pointer' )
    })

    #
    # Download the CodeWAS results table as a csv file
    #
    output$downloadCodeWAS <- downloadHandler(
      filename = function() {
        paste('codewas_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.csv', sep='')
      },
      content = function(file) {
        write.csv(r$codeWASData, file, row.names = FALSE)
      }
    )

    #
    # create a ggiraph plot
    #
    output$codeWASplot <- ggiraph::renderGirafe({
      shiny::req(r$codeWASData)
      shiny::req(r$filteredCodeWASData)
      shiny::req(r$filteredCodeWASData  |>  nrow() > 0)

      df <- r$filteredCodeWASData |>
        dplyr::mutate(fold = log2(meanControls / meanCases)) |>
        dplyr::mutate(direction = ifelse(fold > 0, "up", "down")) |> # n.s. = not significant
        dplyr::mutate(direction = ifelse(pValue > 0.0005, "n.s.", direction)) |>
        dplyr::select(analysisName, covariateName, pValue, fold, direction, oddsRatio) |>
        dplyr::mutate(pLog10 = -log10(pValue)) |>
        dplyr::mutate(fold = ifelse(is.infinite(fold), 0, fold))

      p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = fold, y = pLog10, color = direction)) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(
            tooltip = paste("Analysis: ", analysisName, "<br>",
                            "Covariate: ", covariateName, "<br>",
                            "Fold change: ", signif(fold, digits = 3), "<br>",
                            "p-value: ", signif(pValue, digits = 2), "<br>",
                            "OR: ", signif(oddsRatio, digits = 3), "<br>")
          ),
          hover_nearest = TRUE,
          size = 1.5,
          alpha = 0.4) +
        ggplot2::geom_vline(xintercept = c(-0.6, 0.6), col = "gray", linetype = 'dashed') +
        ggplot2::geom_hline(yintercept = -log10(0.05), col = "gray", linetype = 'dashed') +
        ggplot2::labs(
          x = "Fold change (log2)",
          y = "-log10(p-value)"
        ) +
        ggplot2::scale_color_manual(values = c("up" = "#E41A1C", "down" = "#377EB8", "n.s." = "lightgrey"), guide = "none") +
        ggplot2::theme_minimal()

      gg_girafe <- ggiraph::girafe(ggobj = p,
                      options = list(
                        ggiraph::opts_tooltip(use_fill = TRUE),
                        ggiraph::opts_zoom(min = 0.5, max = 5),
                        ggiraph::opts_sizing(rescale = FALSE),
                        ggiraph::opts_toolbar(saveaspng = TRUE, delay_mouseout = 2000)
                      )
      )

      gg_plot <- ggiraph::girafe(ggobj = p) |> ggiraph::girafe_options(ggiraph::opts_hover(css = "fill: red;"))

      gg_plot <- NULL

      return(gg_plot)
    })


  })
}








