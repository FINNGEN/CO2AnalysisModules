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
#'
#' @export
#'
mod_resultsVisualisation_CodeWAS_ui <- function(id) {
  ns <- shiny::NS(id)

  # this must be in sync with the columns in the reactable table
  tableColumns <- c(
    "Analysis Name" = "analysisName",
    "Concept Code" = "conceptCode",
    "Vocabulary" = "vocabularyId",
    "Domain" = "domainId",
    "N cases" = "nCasesYes",
    "N ctrls" = "nControlsYes",
    "Ratio|Mean cases" = "meanCases",
    "SD cases" = "sdCases",
    "Ratio|Mean ctrls" = "meanControls",
    "SD ctrls" = "sdControls",
    "OR" = "oddsRatio",
    "mlogp" = "mlogp",
    "Beta" = "beta",
    "Model" = "modelType",
    "Notes" = "runNotes"
  )

  shiny::fluidPage(
    title = "CodeWAS Results",
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    shiny::tagList(
      shinyWidgets::chooseSliderSkin("Flat"),
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
          tags$style(HTML("
                         .slider-animate-container,
                          .irs-min, .irs-max, .irs-single {
                              display: none !important;
                          }
                      ")),
          shiny::column(
            width = 2, align = "left",
            shiny::div(style = "margin-top: 30px; ",
                       shiny::checkboxInput(ns("top_10"), "Show labels", value = TRUE),
            ),
          ), # column
          shiny::column(
            width = 2,
            div(style = "margin-top: 10px;",
                div(style = "margin-top: 2px; margin-right: 5px;", "Label top n"),
                div(style = "margin-top: -20px;",
                    shiny::sliderInput(
                      ns("label_top_n"), label = NULL, ticks = FALSE, min = 1, max = 20, value = 10, step = 1)
                )
            )
          ), # column
          shiny::div(style = "height: 100%; width: 100%; ",
                     shinycssloaders::withSpinner(
                       ggiraph::girafeOutput(ns("codeWASplot")),
                       proxy.height = "400px"
                     )
          ),
          shiny::div(
            style = "margin-top: 10px; margin-bottom: 10px;",
            shiny::downloadButton(ns("downloadPlot"), "Download")
          )
        ),
        shiny::tabPanel(
          "Table",
          shiny::div(
            fluidRow(
              column(10,
                     tags$div(style = "display: flex; align-items: center; gap: 15px;",
                              tags$label("Sort by:", style = "width: 50px; margin-bottom: 0;"),
                              tags$div(style = "margin-top: 15px;",
                                       selectInput(ns("sortFirst"), label = NULL, choices = tableColumns, width = "150px", selected = "oddsRatio"),
                              ),
                              tags$div(style = "width: 50px;",
                                       checkboxInput(ns("sortFirstDesc"), "descending", value = TRUE)
                              ),
                              tags$label("", style = "width: 20px; margin-bottom: 0; margin-left: 10px;"),
                              tags$div(style = "margin-top: 15px;",
                                       selectInput(ns("sortSecond"), label = NULL, choices = tableColumns, width = "150px", selected = "mlogp"),
                              ),
                              tags$div(style = "width: 50px;",
                                       checkboxInput(ns("sortSecondDesc"), "descending", value = TRUE)
                              )
                     )
              ),
            ) # fluidRow
          ), # div
          shiny::div(
            style = "margin-top: 0px; margin-bottom: 10px;",
            shinycssloaders::withSpinner(
              reactable::reactableOutput(ns("codeWAStable")),
              proxy.height = "400px"
            )
          ),
          shiny::div(
            style = "margin-top: 10px; margin-bottom: 10px;",
            shiny::downloadButton(ns("downloadCodeWASFiltered"), "Download filtered", icon = shiny::icon("download")),
            shiny::downloadButton(ns("downloadCodeWASAll"), "Download all", icon = shiny::icon("download"))
          )
        )# tabPanel
      ) # tabsetPanel
    ) # tagList
  ) # fluidPage
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

    atlasUrl <- "https://atlas.app.finngen.fi"

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
        tidyr::separate(covariateName, c("domain", "name"), sep = ":", extra = "merge", fill = "right") |>
        dplyr::mutate(covariateName = ifelse(is.na(name), domain, name)) |>
        dplyr::mutate(name = ifelse(is.na(name), domain, name)) |>
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
            databaseId, domainId, conceptCode, vocabularyId, analysisName, covariateId, covariateName, nCasesYes, nControlsYes,
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
    output$codeWAStable <- reactable::renderReactable({
      shiny::req(r$filteredCodeWASData)
      shiny::req(r$filteredCodeWASData  |>  nrow() > 0)
      shiny::req(input$sortFirst)
      shiny::req(input$sortSecond)

      df <- r$filteredCodeWASData |>
        dplyr::mutate(mlogp = round(-log10(pValue), 3)) |>
        dplyr::mutate(oddsRatio = round(oddsRatio, 3)) |>
        dplyr::mutate(beta = round(beta, 3)) |>
        dplyr::mutate(meanCases = round(meanCases, 3)) |>
        dplyr::mutate(sdCases = round(sdCases, 3)) |>
        dplyr::mutate(meanControls = round(meanControls, 3)) |>
        dplyr::mutate(sdControls = round(sdControls, 3)) |>
        dplyr::mutate(covariateId = round(covariateId/1000)) |>
        dplyr::select(
          covariateName, covariateId, conceptCode, vocabularyId, analysisName, domainId,
          nCasesYes, nControlsYes, meanCases, sdCases, meanControls, sdControls,
          oddsRatio, mlogp, beta, modelType, runNotes
        )

      df <- case_when(
          input$sortFirstDesc & input$sortSecondDesc ~
            dplyr::arrange(df, desc(across(all_of(input$sortFirst))), desc(across(all_of(input$sortSecond)))),
          input$sortFirstDesc & !input$sortSecondDesc ~
            dplyr::arrange(df, desc(across(all_of(input$sortFirst))), across(all_of(input$sortSecond))),
          !input$sortFirstDesc & input$sortSecondDesc ~
            dplyr::arrange(df, across(all_of(input$sortFirst)), desc(across(all_of(input$sortSecond)))),
          !input$sortFirstDesc & !input$sortSecondDesc ~
            dplyr::arrange(df, across(all_of(input$sortFirst)), across(all_of(input$sortSecond))),
          TRUE ~ df
        )

      reactable::reactable(
        df,
        filterable = TRUE,
        bordered = TRUE,
        # highlight = TRUE,
        striped = TRUE,
        defaultColDef = reactable::colDef(
          resizable = TRUE
        ),
        # defaultSorted = list(mlogp = "desc", oddsRatio = "desc"),
        sortable = FALSE,
        columns = list(
          covariateName = reactable::colDef(
            name = "Covariate Name",
            cell = function(name, rowIndex) {
              htmltools::tags$a(
                href = paste0(atlasUrl, "/#/concept/", df$covariateId[ rowIndex ]),
                target = "_blank", df$covariateName[ rowIndex ],
                content = name
              )
            },
            minWidth = 50
          ),
          covariateId = reactable::colDef(show = FALSE),
          analysisName = reactable::colDef(name = "Analysis Name", minWidth = 30),
          conceptCode = reactable::colDef(name = "Concept Code", minWidth = 15),
          vocabularyId = reactable::colDef(name = "Vocabulary", minWidth = 15),
          domainId = reactable::colDef(name = "Domain", minWidth = 25),
          nCasesYes = reactable::colDef(name = "N cases", minWidth = 12),
          nControlsYes = reactable::colDef(name = "N ctrls", minWidth = 12),
          meanCases = reactable::colDef(name = "Ratio|Mean cases", minWidth = 13),
          sdCases = reactable::colDef(name = "SD cases", minWidth = 13),
          meanControls = reactable::colDef(name = "Ratio|Mean ctrls", minWidth = 13),
          sdControls = reactable::colDef(name = "SD ctrls", minWidth = 13),
          oddsRatio = reactable::colDef(name = "OR", minWidth = 25),
          mlogp = reactable::colDef(name = "mlogp", minWidth = 25),
          beta = reactable::colDef(name = "Beta", minWidth = 25),
          modelType = reactable::colDef(name = "Model", minWidth = 30),
          runNotes = reactable::colDef(name = "Notes", minWidth = 30)
        ),
        searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE
      ) # reactable

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
    # create a ggiraph plot ####
    #
    output$codeWASplot <- ggiraph::renderGirafe({
      shiny::req(r$codeWASData)
      shiny::req(r$filteredCodeWASData)
      shiny::req(r$filteredCodeWASData  |>  nrow() > 0)

      n_no_test <- sum(grepl("no test", r$filteredCodeWASData, ignore.case = TRUE))
      p_limit <- -log(0.05/(nrow(r$codeWASData) - n_no_test))

      color_coding <- c(
        "cases" = "#E41A1C",
        "controls" = "#377EB8",
        "n.s." = "lightgrey"
      )

      df <- r$filteredCodeWASData |>
        dplyr::mutate(oddsRatio = ifelse(is.na(oddsRatio), 1, oddsRatio)) |>
        dplyr::mutate(pLog10 = -log10(pValue)) |>
        dplyr::mutate(pLog10 = ifelse(is.infinite(pLog10), log10(.Machine$double.xmax), pLog10)) |>
        dplyr::mutate(beta = log(oddsRatio)) |>
        dplyr::mutate(beta = ifelse(beta > 5, 5, beta)) |>
        dplyr::mutate(beta = ifelse(beta < -5, -5, beta)) |>
        dplyr::mutate(direction = case_when(
          pLog10 < p_limit ~ "n.s.",
          beta > 0 ~ "cases",
          beta < 0 ~ "controls",
          TRUE ~ "n.s."
        )) |>
        dplyr::select(analysisName, covariateName, conceptCode, vocabularyId, pValue, oddsRatio, direction, oddsRatio, pLog10, beta, meanCases, meanControls, modelType) |>
        dplyr::mutate(data_id = dplyr::row_number())

      cat("machine double xmax: ", .Machine$double.xmax, "\n")
      cat("machine log10(double xmax): ", log10(.Machine$double.xmax), "\n")

      p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = beta, y = pLog10, color = direction)) +
        # draw a gray rectangle showing the wall of beta = 5
        ggplot2::geom_rect(
          data = NULL,
          xmin = 5.0, xmax = 10, ymin = 0, ymax = p_limit,
          fill = "#EFEFEF", alpha = 1, color = "#EFEFEF"
        ) +
        ggplot2::geom_rect(
          data = NULL,
          xmin = -10, xmax = -5.0, ymin = 0, ymax = log10(.Machine$double.xmax),
          fill = "#EFEFEF", alpha = 1, color = "#EFEFEF"
        ) +
        # draw a gray rectangle showing the highest p-value
        ggplot2::geom_rect(
          data = NULL,
          xmin = -10, xmax = 10, ymin = log10(.Machine$double.xmax), ymax = 400, # ~308
          fill = "#EFEFEF", alpha = 1, color = "#EFEFEF"
        ) +
        # show the p-value and beta limits
        ggplot2::geom_hline(aes(yintercept = p_limit), col = "red", linetype = 'dashed', alpha = 0.3) +
        ggplot2::geom_vline(xintercept = 0, col = "red", linetype = 'dashed', alpha = 0.3) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(
            data_id = data_id,
            tooltip = paste(
              "Covariate: ", covariateName, "<br>",
              "Analysis: ", analysisName, "<br>",
              "Concept code: ", conceptCode, "<br>",
              "Vocabulary: ", vocabularyId, "<br>",
              "beta: ", signif(beta, digits = 3), "<br>",
              "OR: ", signif(oddsRatio, digits = 3), "<br>",
              "p-value: ", signif(pValue, digits = 2), "<br>",
              "mlogp: ", signif(pLog10, digits = 2), "<br>"
            ),
            fill = direction
          ),
          hover_nearest = TRUE,
          shape = 21,
          color = "black",
          size = 1.5,
          alpha = 1.0,
          stroke = 0.2
        ) +
        {if(input$top_10)
          # label the top 10 values
          ggrepel::geom_text_repel(
            data =  df |>
              dplyr::filter(direction == "cases") |>
              dplyr::arrange(desc(beta), desc(pLog10)) |> # this or arrange(desc(pLog10), desc(oddsRatio))?
              dplyr::slice_head(n = input$label_top_n),
            ggplot2::aes(
              label = stringr::str_wrap(stringr::str_trunc(.removeDomain(covariateName), 45), 30)
            ),
            color = "black",
            max.overlaps = Inf,
            force = 0.5,
            force_pull = 0.5,
            size = grid::unit(2.25, "mm"),
            # hjust = 0.1,
            box.padding = grid::unit(3, "mm"),
            segment.linetype = "dashed",
            segment.alpha = 0.25
          )} +
        ggplot2::scale_x_continuous(breaks = function(x) unique(c(-5:-1, 1:5))) +  # Always include -5, 5
        ggplot2::scale_y_continuous(transform = "log10", labels = function(x)round(x,1), expand = ggplot2::expansion(mult = c(0.1, 0.3))) +
        ggplot2::coord_cartesian(xlim = c(min(df$beta) - 1, max(df$beta) + 2), ylim = range(df$pLog10)) +
        ggplot2::scale_fill_manual(name = "Enriched in", values = color_coding) + #, guide = "none") +
        ggplot2::labs(
          x = "beta",
          y = "-log10(p-value)",
          title = paste("Multiple testing significance >", round(p_limit, 1)),
          subtitle = paste("-log( 0.05 / (number of covariates))"),
          caption = "The max numerical magnitude of beta is 5",
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          text = ggplot2::element_text(size = 8),
          panel.grid.minor = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(size = 8),
          plot.subtitle = ggplot2::element_text(size = 6),
          plot.caption = ggplot2::element_text(size = 6, color = "darkgray", margin = ggplot2::margin(t = -10)),
          plot.caption.position = "plot",
          axis.text.x = ggplot2::element_text(size = 7),
          axis.text.y = ggplot2::element_text(size = 7),
          legend.key.height = grid::unit(3, "mm"),
          legend.key.width = grid::unit(7, "mm"),
          legend.title = ggplot2::element_text(size = 7),
          legend.text = ggplot2::element_text(size = 7)
        )

      r$lastPlot <- p

      gg_plot <- ggiraph::girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = 4,
        options = list(
          ggiraph::opts_tooltip(use_fill = FALSE),
          ggiraph::opts_zoom(min = 0.5, max = 5),
          ggiraph::opts_sizing(rescale = TRUE, width = 1),
          ggiraph::opts_toolbar(saveaspng = TRUE, delay_mouseout = 2000),
          ggiraph::opts_hover(
            css = "fill-opacity:1;fill:red;stroke:black;",
            reactive = FALSE
          ),
          ggiraph::opts_toolbar(
            saveaspng = FALSE,
            hidden = c("zoom", "reset", "zoomin", "zoomout", "pan", "lasso", "select", "lasso_select", "lasso_deselect", "box_select", "box_zoom", "reset", "saveaspng"),
          )
        )
      )

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








