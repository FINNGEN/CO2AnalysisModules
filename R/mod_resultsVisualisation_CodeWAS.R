#' @title CodeWAS Results Visualization UI
#' @description UI module for visualizing CodeWAS results. This module provides controls to customize the appearance of the plot and options to download the plot and data.
#'
#' @param id A string representing the module's namespace.
#'
#' @return A Shiny UI element that can be included in a Shiny app.
#'
#' @importFrom shiny NS tagList h4 div uiOutput tabsetPanel tabPanel downloadButton
#' @importFrom htmltools hr
#' @importFrom ggiraph girafeOutput
#' @importFrom DT dataTableOutput
#'
#' @export
#'
mod_resultsVisualisation_CodeWAS_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    title = "CodeWAS Results",
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
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
          # shiny::div(style = "height: 20px;"),
          shiny::column(
            width = 2, align = "left",
            shiny::div(style = "margin-top: 30px; ",
                       shiny::checkboxInput(ns("top_10"), "Label top 10", value = TRUE),
            ),
          ), # column
          shiny::div(style = "height: 100%; width: 800px; margin-left:40px;",
                     ggiraph::girafeOutput(ns("codeWASplot"))
          ),
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
            shiny::downloadButton(ns("downloadCodeWASFiltered"), "Download filtered", icon = shiny::icon("download")),
            shiny::downloadButton(ns("downloadCodeWASAll"), "Download all", icon = shiny::icon("download"))
          )
        )
      )
    )
  )
}


#' @title CodeWAS Results Visualization Server
#' @description Server module for handling the logic of the CodeWAS results visualization UI. This module creates interactive plots and tables based on the analysis results and allows the plot and data to be downloaded.
#'
#' @param id A string representing the module's namespace.
#' @param analysisResults Pooled connection to the analysis results duckdb.
#'
#' @return The module returns server-side logic to generate and manage the CodeWAS results visualization.
#'
#' @importFrom shiny moduleServer reactive req renderUI downloadHandler
#' @importFrom shinyWidgets pickerInput chooseSliderSkin
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom dplyr tbl collect mutate left_join filter select arrange slice_head row_number
#' @importFrom tidyr separate
#' @importFrom stringr str_remove str_trunc str_wrap
#' @importFrom purrr map2_chr
#' @importFrom DT dataTableOutput renderDataTable datatable formatStyle
#' @importFrom ggiraph renderGirafe girafeOutput geom_point_interactive girafe opts_tooltip opts_zoom opts_sizing opts_toolbar opts_hover
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes geom_vline geom_hline scale_x_continuous scale_y_continuous coord_cartesian labs scale_color_manual theme_minimal
#' @importFrom grDevices cairo_pdf dev.off
#' @importFrom lubridate now
#' @importFrom htmltools hr
#' @importFrom grid unit
#'
#' @export
mod_resultsVisualisation_CodeWAS_server <- function(id, analysisResults) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    atlasUrl <- "TEMP" #shiny::getShinyOption("cohortOperationsConfig")$atlasUrl

    # reactive values
    r <- shiny::reactiveValues(
      codeWASData = NULL,
      filteredCodeWASData = NULL,
      lastPlot = NULL
    )

    #
    # load the CodeWAS data
    #
    shiny::observe({
      r$codeWASData <- analysisResults |> dplyr::tbl('codewasResults') |>
        dplyr::left_join(analysisResults |> dplyr::tbl('covariateRef') , by = c('covariateId' = 'covariateId'))  |>
        dplyr::left_join(analysisResults |> dplyr::tbl('analysisRef') , by = c('analysisId' = 'analysisId')) |>
        dplyr::collect() |>
        dplyr::mutate(beta  = log(oddsRatio)) |>
        dplyr::select(-c('isBinary', 'missingMeansZero')) |>
        dplyr::mutate(mplog = cut(-log10(pValue),
                                  breaks = c(0, 5, 100, Inf),
                                  labels = c('-log10(p) (0,5]', '-log10(p) (5,100]', '-log10(p) (100,Inf]'))
        ) |>
        tidyr::separate(covariateName, c("domain", "name"), sep = ":", extra = "merge") |>
        dplyr::mutate(covariateName = ifelse(is.na(name), domain, name)) |>
        dplyr::mutate(covariateName = stringr::str_remove(covariateName, "^[:blank:]")) |>
        dplyr::mutate(domain = stringr::str_remove(domain, "^[:blank:]"))
    })

    #
    # render the CodeWAS filters from the data
    #
    output$codeWASFilter <- shiny::renderUI({
      req(r$codeWASData)

      shiny::tagList(
        shiny::fluidRow(
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
        ), # fluidRow

        shiny::hr(style = "margin-top: 10px; margin-bottom: 5px;"),
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::textInput(
              inputId = ns("p_value_threshold"),
              label =  "p-value threshold",
              value = "1e-5",
              width = "100%"
            ),
          ), # column
          shinyWidgets::chooseSliderSkin("Flat"),
          shiny::column(
            width = 2, align = "left",
            shiny::div(style = "height: 85px; width: 100%; margin-top: -15px;",
                       shiny::sliderInput(ns("or_range"), "OR range filtered out", min = 0.0, max = 2, value = c(0.8,1.2), step = 0.1),
            )
          ),
          shiny::column(
            width = 2, align = "left",
            shiny::div(style = "height: 85px; width: 100%; margin-top: -15px; margin-right: 20px;",
                       shiny::sliderInput(ns("n_cases"), "Minimum # of cases", min = 0, max = 1000, value = 0, step = 1),
            ),
          ), # column
          shiny::column(
            width = 2, align = "left",
            shiny::div(style = "width: 100%; margin-top: 20px; margin-right: 20px;",
                       shiny::checkboxInput(ns("na_anywhere"), "Allow NA", value = FALSE),
            ),
            shiny::div(style = "width: 100%; margin-top: -5px; margin-right: 20px;",
                       shiny::checkboxInput(ns("or_filter_disable"), "Disable OR filter", value = FALSE),
            ),
          ) # column
        ) # fluidRow
      ) # tagList
    })

    is_valid_number <- function(input_string) {
      # This regex matches regular and scientific notation numbers
      return(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", input_string))
    }

    #
    # filter the data
    #
    shiny::observe({
      shiny::req(r$codeWASData)
      shiny::req(input$or_range)
      shiny::req(input$n_cases)
      shiny::isTruthy(input$na_anywhere)

      if(input$p_value_threshold == "") {
        shinyFeedback::showFeedbackWarning(
          inputId = "p_value_threshold",
          text = "Invalid input: Please give a valid number (default is 1e-5)."
        )
        } else if(!is_valid_number(input$p_value_threshold)) {
          shinyFeedback::showFeedbackWarning(
            inputId = "p_value_threshold",
            text = "Invalid input: Please give a valid number."
          )
        } else if(as.numeric(input$p_value_threshold) < 0) {
        shinyFeedback::showFeedbackWarning(
          inputId = "p_value_threshold",
          text = "Invalid input: Please give a number greater than 0."
        )
      } else if(as.numeric(input$p_value_threshold) > 1) {
        shinyFeedback::showFeedbackWarning(
          inputId = "p_value_threshold",
          text = "Invalid input: Please give a number between 0 and 1."
        )
      } else {
        shinyFeedback::hideFeedback("p_value_threshold")
        # filter the data
        r$filteredCodeWASData <- r$codeWASData |>
          dplyr::select(
            databaseId, domainId, analysisName, covariateId, covariateName, nCasesYes, nControlsYes,
            meanCases, sdCases, meanControls, sdControls, oddsRatio, pValue, beta, modelType, runNotes
          ) |>
          dplyr::filter(
            if (!is.null(input$domain)) domainId %in% input$domain else FALSE,
            if (!is.null(input$analysis)) analysisName %in% input$analysis else FALSE,
            if (!is.null(input$model)) modelType %in% input$model else FALSE
          )  |>
          dplyr::filter(
            as.double(pValue) <= (as.double(input$p_value_threshold) + 2 * .Machine$double.eps)
            | is.na(pValue)
          ) |>
          dplyr::filter(
            as.double(oddsRatio) <= as.double(input$or_range[1])
            | as.double(oddsRatio) >= as.double(input$or_range[2])
            | input$or_filter_disable
            | is.na(oddsRatio)
          ) |>
          dplyr::filter(nCasesYes >= input$n_cases)  |>
          dplyr::filter(!dplyr::if_any(c("pValue", "oddsRatio"), is.na) | input$na_anywhere)
      }
    })

    #
    # toggle the OR filter
    #
    shiny::observe({
      shiny::isTruthy(input$or_filter_disable)
      shiny::req(input$or_range)

      if(input$or_filter_disable) {
        shinyjs::disable("or_range")
      } else {
        shinyjs::enable("or_range")
      }
     })

    #
    # render the CodeWAS table
    #
    output$codeWAStable <- DT::renderDataTable({
      shiny::req(r$filteredCodeWASData)
      shiny::req(r$filteredCodeWASData  |>  nrow() > 0)

      # https://github.com/rstudio/DT/issues/1127
      # the bug can be worked around by setting shiny.json.digits to a smaller value
      options(shiny.json.digits = 4)

      DT::datatable(
        r$filteredCodeWASData |>
          dplyr::mutate(pValue = round(pValue, 3)) |>
          dplyr::mutate(oddsRatio = round(oddsRatio, 3)) |>
          dplyr::mutate(beta = round(beta, 3)) |>
          dplyr::mutate(meanCases = round(meanCases, 3)) |>
          dplyr::mutate(sdCases = round(sdCases, 3)) |>
          dplyr::mutate(meanControls = round(meanControls, 3)) |>
          dplyr::mutate(sdControls = round(sdControls, 3)) |>
          dplyr::mutate(covariateNameFull = as.character(covariateName)) |>
          dplyr::mutate(covariateName = stringr::str_trunc(covariateName, 50)) |>
          dplyr::mutate(analysisName = stringr::str_trunc(analysisName, 20)) |>
          dplyr::mutate(
            covariateId = round(covariateId/1000),
            covariateName = purrr::map2_chr(covariateName, covariateId, ~paste0('<a href="',atlasUrl,'/#/concept/', .y, '" target="_blank">', .x,'</a>'))
          ) |>
          dplyr::select(
            covariateName, analysisName, domainId,
            nCasesYes, nControlsYes, meanCases, sdCases, meanControls, sdControls,
            oddsRatio, pValue, beta, modelType, runNotes
          ),
        escape = FALSE,
        class = 'display nowrap compact',
        selection = 'none',
        rownames = FALSE,
        colnames = c(
          # 'Database' = 'databaseId',
          'Covariate Name' = 'covariateName',
          'Analysis Name' = 'analysisName',
          'Domain' = 'domainId',
          # 'Concept ID' = 'conceptId',
          # 'Cov. ID' = 'covariateId',
          # 'N tot' = 'n_total',
          # 'Type' = 'upIn',          # upIn MISSING from data
          'N cases' = 'nCasesYes',
          'N ctrls' = 'nControlsYes',
          'Ratio|Mean cases' = 'meanCases',
          'Ratio|Mean ctrls' = 'meanControls',
          'SD cases' = 'sdCases',
          'SD ctrls' = 'sdControls',
          'OR' = 'oddsRatio',
          'p' = 'pValue',
          'Beta' = 'beta',
          'Model' = 'modelType',
          # 'ID' = 'analysisId',
          'Notes' = 'runNotes'
          # 'Binary' = 'isBinary',
          # 'Missing mean zero' = 'missingMeansZero'
          # 'covariateNameFull' = 'covariateNameFull'
        ),
        options = list(
          order = list(list(10, 'asc'), list(9, 'desc')), # pValue, OR
          pageLength = 20,
          lengthMenu = c(10, 15, 20, 25, 30)
        )
      ) |>
        DT::formatStyle('Covariate Name', cursor = 'pointer' ) |>
        DT::formatSignif(columns = c('p', 'OR'), digits = 3)
    })

    #
    # Download the CodeWAS results table as a csv file
    #
    output$downloadCodeWASFiltered <- downloadHandler(
      filename = function() {
        paste('codewas_filtered_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.csv', sep='')
      },
      content = function(file) {
        write.csv(r$filteredCodeWASData, file, row.names = FALSE)
      }
    )

    #
    # Download the CodeWAS results table as a csv file
    #
    output$downloadCodeWASAll <- downloadHandler(
      filename = function() {
        paste('codewas_all_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.csv', sep='')
      },
      content = function(file) {
        write.csv(r$codeWASData, file, row.names = FALSE)
      }
    )

    # helper to remove the domain from the covariate name
    .removeDomain <- function(s) {
      return(gsub(".*:", "", s))
    }

    #
    # create a ggiraph plot
    #
    output$codeWASplot <- ggiraph::renderGirafe({
      shiny::req(r$codeWASData)
      shiny::req(r$filteredCodeWASData)
      shiny::req(r$filteredCodeWASData  |>  nrow() > 0)

      df <- r$filteredCodeWASData |>
        dplyr::mutate(oddsRatio = ifelse(is.na(oddsRatio), 1, oddsRatio)) |>
        dplyr::mutate(pLog10 = -log10(pValue)) |>
        dplyr::mutate(beta = log(oddsRatio)) |>
        dplyr::mutate(beta = ifelse(beta > 5, 5, beta)) |>
        dplyr::mutate(beta = ifelse(beta < -5, -5, beta)) |>
        dplyr::mutate(direction = ifelse(beta > 0, "cases", "controls")) |> # n.s. = not significant
        dplyr::select(analysisName, covariateName, pValue, oddsRatio, direction, oddsRatio, pLog10, beta, meanCases, meanControls, modelType) |>
        dplyr::mutate(data_id = dplyr::row_number())

      # browser()
      n_no_test <- sum(grepl("no test", df$modelType, ignore.case = TRUE))
      p_limit <- -log(0.05/(nrow(df) - n_no_test))

      p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = beta, y = pLog10, color = direction)) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(
            data_id = data_id,
            tooltip = paste("Analysis: ", analysisName, "<br>",
                            "Covariate: ", covariateName, "<br>",
                            "beta: ", signif(beta, digits = 3), "<br>",
                            "OR: ", signif(oddsRatio, digits = 3), "<br>",
                            "p-value: ", signif(pValue, digits = 2), "<br>"
            )
          ),
          hover_nearest = TRUE,
          size = 1.5,
          alpha = 0.4) +
        # show the p-value limit
        ggplot2::geom_hline(aes(yintercept = p_limit), col = "red", linetype = 'dashed') +
        {if(input$top_10)
          # label the top 10 values
          ggrepel::geom_text_repel(
            data =  df |>
              dplyr::arrange(pValue, oddsRatio) |>
              dplyr::slice_head(n = 10),
            ggplot2::aes(
              label = stringr::str_wrap(stringr::str_trunc(.removeDomain(covariateName), 45), 30),
              color = "grey",
            ),
            max.overlaps = Inf,
            force = 1,
            size = grid::unit(3, "mm"),
            # hjust = 0.1,
            box.padding = grid::unit(3, "mm")
          )} +
        ggplot2::geom_vline(xintercept = 0, col = "red", linetype = 'dashed') +
        ggplot2::scale_x_continuous() +
        ggplot2::scale_y_continuous(transform = "log10", labels = function(x)round(x,1)) +
        # ggplot2::coord_cartesian(xlim = range(df$beta), ylim = range(df$pLog10)) +
        ggplot2::coord_cartesian(xlim = c(-5, 5), ylim = range(df$pLog10)) +
        ggplot2::labs(
          x = "beta",
          y = "-log10(p-value)",
          color = "Enriched in",
          title = "",
          subtitle = paste("-log( 0.05 / (number of rows)) = ", round(p_limit, 1))
        ) +
        ggplot2::scale_color_manual(values = c("cases" = "#E41A1C", "controls" = "#377EB8", "n.s." = "lightgrey")) + #, guide = "none") +
        ggplot2::theme_minimal()

      r$lastPlot <- p

      gg_plot <- ggiraph::girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = 4,
        options = list(
          ggiraph::opts_tooltip(use_fill = FALSE),
          ggiraph::opts_zoom(min = 0.5, max = 5),
          ggiraph::opts_sizing(rescale = FALSE, width = 1),
          ggiraph::opts_toolbar(saveaspng = TRUE, delay_mouseout = 2000),
          ggiraph::opts_hover(css = "fill: black;"),
          ggiraph::opts_toolbar(
            saveaspng = FALSE,
            hidden = c("zoom", "reset", "zoomin", "zoomout", "pan", "lasso", "select", "lasso_select", "lasso_deselect", "box_select", "box_zoom", "reset", "saveaspng"),
          )
        )
      )

      # gg_plot <- ggiraph::girafe(ggobj = p) |> ggiraph::girafe_options(ggiraph::opts_hover(css = "fill: red;"))

      # gg_plot <- NULL

      return(gg_plot)
    })

    #
    # download data as a plot
    #
    output$downloadPlot <- shiny::downloadHandler(
      filename = function(){
        paste('codewas_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.pdf', sep='')
      },
      content = function(fname){

        grDevices::cairo_pdf(filename = fname,
                             width = 7,
                             height = 5,
                             pointsize = 1.0,
                             family = "sans",
                             bg = "transparent",
                             antialias = "default",
                             fallback_resolution = 300,
        )
        print(r$lastPlot)
        grDevices::dev.off()
      },
      contentType = "application/pdf"
    )


  })
}








