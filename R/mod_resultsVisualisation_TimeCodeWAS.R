#' @title Cohort TimeCodeWAS Visualization UI
#' @description UI module for visualizing cohort overlaps using an UpSet plot. This module provides controls to customize the appearance of the plot and options to download the plot and data.
#'
#' @param id A string representing the module's namespace.
#'
#' @return A Shiny UI element that can be included in a Shiny app.
#'
#' @importFrom shiny NS tagList tags h4 uiOutput tabsetPanel tabPanel div downloadButton
#' @importFrom ggiraph girafeOutput
#' @importFrom DT DTOutput
#'
#' @export
#'
mod_resultsVisualisation_TimeCodeWAS_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    title = "TimeCodeWAS",
    shiny::tagList(
      shinyWidgets::chooseSliderSkin("Flat"),
      shiny::tags$h4("Filters"),
      shiny::uiOutput(ns("outputUI")),
      shiny::tags$h4("Data"),
      shiny::tabsetPanel(
        id = ns("tabset"),
        shiny::tabPanel(
          "Proportions View",
          shiny::div(style = "margin-top: 0px; ",
                     shiny::fluidRow(
                       tags$style(HTML("
                         .slider-animate-container,
                          .irs-min, .irs-max, .irs-single {
                              display: none !important;
                          }
                      ")),
                       shiny::column(
                         width = 2,
                         shiny::div(style = "margin-top: 25px; ",
                                    shiny::checkboxInput(
                                      ns("top_10"), "Show labels", value = TRUE)
                         )
                       ),
                       shiny::column(
                         width = 2,
                         div(style = "margin-top: 10px;",
                             div(style = "margin-top: 5px; margin-right: 5px;", "Label top n"),
                             div(style = "margin-top: -20px;",
                                 shiny::sliderInput(
                                   ns("label_top_n"), label = NULL, ticks = FALSE, min = 1, max = 20, value = 10, step = 1)
                             )
                         )
                       ), # column
                       shiny::column(
                         width = 2,
                         div(style = "margin-top: 10px;",
                             div(style = "margin-top: 5px; margin-right: 5px;", "Point size"),
                             div(style = "margin-top: -20px;",
                                 shiny::sliderInput(
                                   ns("point_scale"), label = NULL, ticks = FALSE, min = 0.2, max = 2.2, value = 1, step = 0.1)
                             )
                         )
                       ), # column
                       shiny::column(
                         width = 2,
                         div(style = "margin-top: 10px;",
                             div(style = "margin-top: 5px; margin-right: 5px;"),
                             div(style = "margin-top: 20px;",
                                 shiny::actionButton(
                                   ns("unselect"), label = "Unselect")
                             )
                         )
                       ), # column
                     )
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput(ns("proportionsView"), width = "100%", height = "100%"),
            proxy.height = "400px"
          ),
          shiny::div(
            style = "margin-top: 10px; margin-bottom: 10px;",
            shiny::downloadButton(ns("downloadProportionsView"), "Download")
          ),
        ),
        shiny::tabPanel(
          "Progress View",
          shiny::div(style = "margin-top: 10px; ",
                     shiny::fluidRow(
                       shiny::column(
                         width = 2,
                         shiny::div(style = "margin-top: 25px; ",
                                    shiny::checkboxInput(
                                      ns("show_labels"), "Show labels", value = TRUE)
                         )
                       ),
                       shiny::column(
                         width = 2,
                         shiny::div(style = "margin-top: 25px; ",
                                    shiny::checkboxInput(
                                      ns("connect_dots"), "Connect points", value = TRUE)
                         )
                       ),
                       shiny::column(
                         width = 2,
                         div(style = "margin-top: 10px;",
                             div(style = "margin-top: 5px; margin-right: 5px;", "Limit points"),
                             div(style = "margin-top: -20px;",
                                 shiny::sliderInput(
                                   ns("label_limit"), label = NULL, ticks = FALSE, min = 1, max = 20, value = 10, step = 1)
                             )
                         )
                       ), # column
                     )
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput(ns("progressView"), width = "100%", height = "100%"),
            proxy.height = "400px"
          ),
          shiny::div(
            style = "margin-top: 10px; margin-bottom: 10px;",
            shiny::downloadButton(ns("downloadProgressView"), "Download")
          ),
        ),
        shiny::tabPanel(
          "Table",
          shiny::div(
            style = "margin-top: 20px; margin-bottom: 10px;",
            shinycssloaders::withSpinner(
              reactable::reactableOutput(ns("reactableData")),
              proxy.height = "400px"
            )
          ),
          shiny::div(
            style = "margin-top: 10px; margin-bottom: 10px;",
            shiny::downloadButton(ns("downloadDataFiltered"), "Download filtered"),
            shiny::downloadButton(ns("downloadDataAll"), "Download all"),
          )
        ) # tabPanel
      ) # tabsetPanel
    ) # tagList
  ) # fluidPage

}

#' @title Cohort Demographics Visualization Server
#' @description Server module for handling the logic of the cohort overlaps visualization UI. This module creates an UpSet plot based on the analysis results and allows the plot and data to be downloaded.
#'
#' @param id A string representing the module's namespace.
#' @param analysisResults Pooled connection to the analysis results duckdb.
#'
#' @return The module returns server-side logic to generate and manage the cohort overlaps UpSet plot.
#'
#' @importFrom shiny moduleServer reactive req renderUI observeEvent downloadHandler
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom shinyWidgets pickerInput pickerOptions chooseSliderSkin
#' @importFrom ggiraph renderGirafe girafe opts_sizing opts_hover opts_selection opts_toolbar geom_point_interactive
#' @importFrom ggrepel geom_text_repel
#' @importFrom DT renderDataTable datatable formatSignif formatStyle
#' @importFrom dplyr filter mutate select arrange transmute left_join pull case_when if_else inner_join row_number
#' @importFrom tidyr separate
#' @importFrom stringr str_remove_all str_remove str_c str_wrap str_trunc str_split str_extract_all str_sub
#' @importFrom scales percent number rescale
#' @importFrom lubridate now days
#' @importFrom readr write_csv
#' @importFrom purrr map2_chr
#' @importFrom grid unit gpar
#' @importFrom gtable gtable_add_grob
#' @importFrom ggplot2 ggplot aes geom_segment scale_size_manual scale_x_continuous scale_y_continuous facet_grid theme_minimal theme scale_color_manual scale_fill_discrete guides labs ggplot_build ggplot_gtable
#' @importFrom ggplotify as.ggplot
#' @importFrom grDevices cairo_pdf dev.off
#' @importFrom shinyjs toggle
#'
#' @export
#'
mod_resultsVisualisation_TimeCodeWAS_server <- function(id, analysisResults) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    atlasUrl <- "https://atlas.app.finngen.fi"

    # reactlogShow
    # options(shiny.reactlog = TRUE)

    # reactive values
    r <- shiny::reactiveValues(
      # data and filtered data
      timeCodeWASData = NULL,
      filteredTimeCodeWASData = NULL,
      timePeriods = NULL,
      # copies of views to save as PDF
      savedProportionsView = NULL,
      savedProgressView = NULL,
      # selection in the Proportions View
      line_to_plot = NULL,
      force_update = FALSE,
    )

    ParallelLogger::logInfo("TimeCodeWAS server started")

    #
    # load the TimeCodeWAS data
    #
    shiny::observe({
      shiny::req(analysisResults)

      timeCodeWASData <- analysisResults |> dplyr::tbl("timeCodeWASResults")  |>
        dplyr::left_join(
          analysisResults |> dplyr::tbl("timeRef") |>
            dplyr::select(timeId, startDay,endDay),
          by = "timeId"
        ) |>
        dplyr::left_join(
          analysisResults |> dplyr::tbl("covariateRef") ,
          by = "covariateId"
        )|>
        dplyr::left_join(
          analysisResults |> dplyr::tbl("analysisRef") ,
          by = "analysisId"
        )  |>
        dplyr::mutate(
          upIn = dplyr::if_else(oddsRatio>1, "Case", "Ctrl"),
          nCasesInWindow = nCasesInWindow,
          nControlsInWindow = nControlsInWindow
        ) |>
        dplyr::collect()

      timeRange <- .get_time_periods(timeCodeWASData)

      timeCodeWASData <- timeCodeWASData |>
        dplyr::left_join(timeRange, by = c("startDay", "endDay"))

      r$timePeriods <- timeRange |> dplyr::pull(timeRange)

      timeCodeWASData <- timeCodeWASData |>
        dplyr::mutate(oddsRatio = dplyr::if_else(is.na(oddsRatio), Inf, oddsRatio)) |>
        dplyr::rename(
          covariateId = covariateId,
          timeId = timeId,
          timeRange = timeRange,
          covariateName = covariateName,
          p = pValue,
          OR = oddsRatio
        )

      timeCodeWASData <- timeCodeWASData |>
        dplyr::transmute(
          code = covariateId,
          time_period = factor(timeRange, levels = r$timePeriods, labels = r$timePeriods),
          name = covariateName,
          conceptCode = conceptCode,
          vocabularyId = vocabularyId,
          analysisName = analysisName,
          model = modelType,
          notes = runNotes,
          OR=OR,
          p=p,
          upIn=upIn,
          nCasesInWindow = nCasesInWindow,
          nControlsInWindow = nControlsInWindow,
          cases_per = nCasesYes/nCasesInWindow,
          meanCases = nCasesYes/nCasesInWindow,
          meanControls = nControlsYes/nControlsInWindow,
          sdCases = sdCases,
          sdControls = sdControls,
          controls_per = nControlsYes/nControlsInWindow,
          nCasesYes = nCasesYes,
          nControlsYes = nControlsYes
        ) |>
        tidyr::separate(name, c("domain", "name"), sep = ":", extra = "merge") |>
        dplyr::mutate(name = stringr::str_remove(name, "^[:blank:]")) |>
        dplyr::mutate(p = dplyr::if_else(p==0, 10^-323, p))

      timeCodeWASData <- timeCodeWASData |>
        dplyr::arrange(time_period, name) |>
        dplyr::mutate_if(is.character, stringr::str_replace_na, "") |>
        dplyr::mutate(
          GROUP = time_period,
          label = stringr::str_c(code),
          label = stringr::str_remove(label, "[:blank:]+$"),
          label = stringr::str_c(domain, " : ", name,
                                 "\n analysis: ", analysisName,
                                 "\n concept code: ", conceptCode,
                                 "\n vocabulary: ", vocabularyId,
                                 "\n -log10(p) = ", scales::number(-log10(p), accuracy = 0.1) ,
                                 "\n log10(OR) = ", ifelse(is.na(OR), "", scales::number(log10(OR), accuracy = 0.1)),
                                 "\n cases: ", nCasesYes, " (", scales::percent(cases_per, accuracy = 0.01), ")",
                                 "\n controls: ", nControlsYes, " (", scales::percent(controls_per, accuracy = 0.01), ")"
          ),
          link = paste0("https://atlas.app.finngen.fi/#/concept/", stringr::str_sub(code, 1, -4)),
          upIn = upIn,
          id = dplyr::row_number(),
          p_group = cut(-log10(p),
                        breaks = c(-1, 50, 100, 200, Inf ),
                        labels = c("-log10(p) [0,50]", "-log10(p) (50,100]", "-log10(p) (100,200]", "-log10(p) (200,Inf]"),
                        ordered_result = TRUE
          ),
          p_group_size = dplyr::case_when(
            as.integer(p_group)==1 ~ 1L,
            as.integer(p_group)==2 ~ 5L,
            as.integer(p_group)==3 ~ 10L,
            as.integer(p_group)==4 ~ 20L,
          ),
          log10_OR = dplyr::case_when(
            log10(OR) == -Inf ~ -2.5,
            log10(OR) == Inf ~ 5,
            TRUE ~ log10(OR) ,
          ),
          data_id = paste0(code, "@", as.character(time_period)),
          data_id_class = code
        ) |>
        dplyr::filter(!is.na(time_period))

      r$timeCodeWASData <- timeCodeWASData
    })

    #
    # render the timeCodeWAS filters from the data
    #
    output$outputUI <- shiny::renderUI({
      shiny::req(r$timeCodeWASData)

      shiny::tagList(
        shinyFeedback::useShinyFeedback(),
        shiny::fluidRow(
          shiny::column(
            width = 2,
            shinyWidgets::pickerInput(
              ns("domain"),
              "Domain",
              choices = unique(r$timeCodeWASData$domain),
              selected = unique(r$timeCodeWASData$domain),
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
              choices = unique(r$timeCodeWASData$analysisName),
              selected = unique(r$timeCodeWASData$analysisName),
              multiple = TRUE,
              options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3", `count-selected-text` = "{0} analyses selected")
            )),
          shiny::column(
            width = 2,
            shinyWidgets::pickerInput(
              ns("model"),
              "Model",
              choices = unique(r$timeCodeWASData$model),
              selected = unique(r$timeCodeWASData$model),
              multiple = TRUE,
              options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3", `count-selected-text` = "{0} model types selected")
            )
          ), # column
          shiny::column(
            width = 2,
            shinyWidgets::pickerInput(
              ns("time_period"),
              "Time periods",
              choices = r$timePeriods,
              selected = r$timePeriods,
              multiple = TRUE,
              options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3", `count-selected-text` = "{0} time periods selected")
            )
          ) # column
        ), # fluidRow
        shiny::div(style = "margin-top: 20px; margin-bottom: 10px;"),
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
    # filter the data ####
    #
    shiny::observe({
      shiny::req(r$timeCodeWASData)
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
        r$filteredTimeCodeWASData <- r$timeCodeWASData |>
          dplyr::filter(
            # if (!is.null(input$database)) databaseId %in% input$database else FALSE,
            if (!is.null(input$domain)) domain %in% input$domain else FALSE,
            if (!is.null(input$analysis)) analysisName %in% input$analysis else FALSE,
            if (!is.null(input$model)) model %in% input$model else FALSE
          ) |>
          dplyr::filter(
            as.double(p) <= (as.double(input$p_value_threshold) + 2 * .Machine$double.eps)
            | is.na(p)
          ) |>
          dplyr::filter(
            as.double(OR) <= as.double(input$or_range[1])
            | as.double(OR) >= as.double(input$or_range[2])
            | input$or_filter_disable
            | is.na(OR)
          ) |>
          dplyr::filter(nCasesYes >= input$n_cases)  |>
          dplyr::filter(!dplyr::if_any(c("p", "OR"), is.na) | input$na_anywhere) |>
          dplyr::filter(!is.null(input$time_period) & time_period %in% input$time_period)
      }
    })

    #
    # remove lines from the Proportions View
    #
    shiny::observe({
      shiny::req(input$time_period)
      shiny::req(input$domain)
      shiny::req(input$analysis)

      r$line_to_plot <- NULL
    })

    #
    # unselect for the Proportions View
    #
    shiny::observe({
      shiny::req(input$unselect)

      r$line_to_plot <- NULL
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
    # Proportions View ####
    #
    output$proportionsView <- ggiraph::renderGirafe({
      shiny::req(r$filteredTimeCodeWASData)
      r$force_update

      gg_data <- r$filteredTimeCodeWASData

      if(nrow(gg_data) == 0){
        return(NULL)
      }
      # adjust the label area according to facet width
      facet_max_x <- max( gg_data$controls_per, 0.03, na.rm = TRUE)
      facet_max_y <- max( gg_data$cases_per, 0.03, na.rm = TRUE)
      #
      #
      gg_fig <- ggplot2::ggplot(
        data = dplyr::arrange( gg_data, log10_OR),
        ggplot2::aes(
          y = cases_per, #log10_OR, # cases_per-controls_per,#log10_OR,# -log10(p), cases_per-controls_per,#
          x = controls_per, #-log10(p), # 1, # id, #log10_OR,
          color = "darkgray",
          fill = domain,
          tooltip = label,
          # size = ordered(p_group), # log10_OR
          data_id = data_id
          # onclick = paste0('window.open("', link , '")')
        ), alpha = 0.75)+
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, y = 0,
                       xend = ifelse(facet_max_x > facet_max_y, facet_max_y, facet_max_x),
                       yend = ifelse(facet_max_x > facet_max_y, facet_max_y, facet_max_x)
          ),
          color = "blue", alpha = 0.5, linewidth = 0.2, linetype = "solid") +
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, y = 0, xend = facet_max_x, yend = 0),
          color = "black", alpha = 0.5, linewidth = 0.2, linetype = "solid") +
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, y = 0, xend = 0, yend = facet_max_y),
          color = "black", alpha = 0.5, linewidth = 0.2, linetype = "solid") +
        {if(length(r$line_to_plot) > 1)
          # label the selected points
          ggrepel::geom_text_repel(
            data =  gg_data |>
              dplyr::filter(data_id %in% r$line_to_plot$data_id),
            ggplot2::aes(label = stringr::str_wrap(stringr::str_trunc(name, 50), 15)),
            max.overlaps = Inf,
            size = 3,
            hjust = 0.1,
            force = 0.5,
            force_pull = 0.5,
            color = "black",
            xlim = c(facet_max_x / 4, NA),
            box.padding = 0.8,
            segment.linetype = "dashed"
          )} +
        {if((length(r$line_to_plot) == 0) & input$top_10)
          # label the top 10 values
          ggrepel::geom_text_repel(
            data =  gg_data |>
              # dplyr::group_by(GROUP) |>
              dplyr::arrange(p, OR) |>
              dplyr::slice_head(n = input$label_top_n), # |>
            # dplyr::ungroup(),
            ggplot2::aes(label = stringr::str_wrap(stringr::str_trunc(name, 45), 30)),
            max.overlaps = Inf,
            size = 3,
            hjust = 0.1,
            force = 1.5,
            force_pull = 0.5,
            xlim = c(facet_max_x / 4, NA),
            box.padding = 0.8,
            color = "black",
            segment.linetype = "dashed"
          )} +
        ggiraph::geom_point_interactive(
          ggplot2::aes(size = p_group), show.legend=T, shape = 21, stroke = 0.2, color = "black"
        ) + #, position = position_dodge(width = 12))+
        ggplot2::scale_size_manual(
          values = c(
            "-log10(p) [0,50]" = 2 * input$point_scale,
            "-log10(p) (50,100]" = 3 * input$point_scale,
            "-log10(p) (100,200]" = 4 * input$point_scale,
            "-log10(p) (200,Inf]" = 6 * input$point_scale
          )
        ) +
        ggplot2::scale_x_continuous(
          breaks = c(0, 0.05, seq(0.1, 0.8, 0.1)),
          labels = c(0, 5, seq(10, 80, 10)),
          limits = c(-0.02 * facet_max_x, facet_max_x)
        ) +
        ggplot2::scale_y_continuous(
          breaks = c(0, 0.05, seq(0.1, 0.8, 0.1)),
          labels = c(0, 5, seq(10, 80, 10)),
          limits = c(-0.02 * facet_max_y, facet_max_y + 0.1 * facet_max_y),
          expand = ggplot2::expansion(mult = c(0.1, 0.1))
        ) +
        # ggplot2::coord_fixed() +
        ggplot2::facet_grid(
          .~GROUP, drop = TRUE, scales = "fixed",
          labeller = ggplot2::labeller(GROUP = .label_editor)
        )+
        ggplot2::theme_minimal()+
        ggplot2::theme(
          legend.key.height = grid::unit(3, "mm"),
          legend.key.width = grid::unit(7, "mm"),
          legend.title = ggplot2::element_text(size = 8),
          legend.text = ggplot2::element_text(size = 8),
          legend.position = "bottom",
          legend.direction = "vertical",
          strip.text.x = ggplot2::element_text(size = 10)
        ) +
        ggplot2::scale_color_manual(values = c("darkgray")) +
        ggplot2::scale_fill_discrete() +
        ggplot2::guides(color = "none", fill = ggplot2::guide_legend(override.aes = list(size = 5), nrow=2, byrow=TRUE)) +
        ggplot2::labs(
          title = "",
          subtitle = "",
          caption = "Blue line indicates the equal proportion of cases and controls",
          size = "p value group",
          fill = "Domain",
          x = "\nControls %",
          y = "Cases %"
        )

      r$savedProportionsView <- gg_fig

      selected_items <- ""

      line_to_plot <- r$line_to_plot

      if(!is.null(line_to_plot) && length(unique(line_to_plot$code)) == 1){
        grDevices::pdf(NULL)
        # one point selected -> draw a line connecting the same code in each facet
        gb <- ggplot2::ggplot_build(gg_fig)
        g <- ggplot2::ggplot_gtable(gb)
        # remove domains not in the current data
        line_to_plot <- line_to_plot |>
          dplyr::filter(domain %in%  line_to_plot$domain)
        # check if we have lines to draw
        if(nrow(line_to_plot) > 1){
          z_val = 0
          ranges <- gb$layout$panel_params
          data2npc <- function(x, range) scales::rescale(c(range, x), c(0,1))[-c(1,2)]
          x_range <-  ranges[[1]][["x.range"]]
          y_range <- ranges[[1]][["y.range"]]

          line_to_plot <- dplyr::inner_join(line_to_plot, g$layout, by = "name") |>
            dplyr::mutate(controls_per = data2npc(controls_per, x_range)) |>
            dplyr::mutate(cases_per = data2npc(cases_per, y_range))
          line_to_plot$z <- 1
          line_to_plot$clip <- "off"

          # move to the beginning of selection
          g <- gtable::gtable_add_grob(
            g, grid::moveToGrob(line_to_plot[1,]$controls_per, line_to_plot[1,]$cases_per),
            t = line_to_plot[1,]$t, line_to_plot[1,]$l, z = z_val)
          # draw the lines
          for(i in 2:nrow(line_to_plot)){
            if(is.na(line_to_plot[i,]$t) || is.na(line_to_plot[i,]$l))
              next
            g <- gtable::gtable_add_grob(
              g, grid::lineToGrob(line_to_plot[i,]$controls_per, line_to_plot[i,]$cases_per, gp = grid::gpar(col = "red", alpha = 0.3, lwd = 2.5)),
              t = line_to_plot[i,]$t, line_to_plot[i,]$l, z = z_val)
          }

          # turn clip off to see the line across panels
          g$layout$clip <- "off"
        }

        # browser()

        if(!is.null(line_to_plot) & length(line_to_plot) == 1){
          selected_items <- as.character(unique(line_to_plot$code))
          # extend selection to same code in all facets
          selected_items <- line_to_plot |>
            dplyr::filter(code == selected_items) |>
            dplyr::pull(data_id)
          skip_selection <- TRUE
          r$line_to_plot <- NULL
        } else {
          selected_items <- ""
          skip_selection <- FALSE
        }

        gg_plot <- ggplotify::as.ggplot(g)
      } else {
        skip_selection <- FALSE
        gg_plot <- gg_fig
      }

      # convert ggplot to girafe object
      gg_girafe <- ggiraph::girafe(ggobj = gg_plot, width_svg = 15, height_svg = 6)

      # modify girafe object
      gg_girafe <- ggiraph::girafe_options(
        gg_girafe,
        ggiraph::opts_sizing(rescale = TRUE, width = 1.0),
        ggiraph::opts_hover(
          css = "fill-opacity:1;fill:red;stroke:black;",
          reactive = FALSE
        ),
        ggiraph::opts_selection(
          type = c("multiple"),
          only_shiny = TRUE,
          selected = ifelse(skip_selection == TRUE, character(0), selected_items)
        ),
        ggiraph::opts_toolbar(
          position = "topright",
          hidden = c("zoom", "zoomReset", "lasso_deselect", "saveaspng"),
          delay_mouseout = 100000
        )
      )
      return(gg_girafe)
    })

    #
    # triggered when points selected by click or by lasso
    # when plot is redrawn this is also triggered
    # this block captures the new selected points and then updates r$selected
    #
    # proportionsView_selected ####
    #
    shiny::observeEvent(input$proportionsView_selected, {
      shiny::req(input$proportionsView_selected)
      shiny::req(input$proportionsView_selected != "NA")

      gg_data <- r$filteredTimeCodeWASData

      # clean selection value take only last selected
      selected_rows <- input$proportionsView_selected
      selected_rows <- selected_rows[selected_rows != ""]
      selected_rows <- selected_rows[selected_rows != "NA"]

      # ignore selected rows that are currently plot as a line
      selected_rows  <- setdiff(selected_rows, r$line_to_plot$data_id)

      # was the line unselected?
      if(length(selected_rows) == 0){
        r$line_to_plot <- NULL
        return()
      }

      line_to_plot <- NULL

      if(length(selected_rows) > 1){
        # we have a marquee selection with n > 1
        df_lasso <- gg_data |>
          dplyr::filter(data_id %in% selected_rows)

        # show table
        shiny::showModal(
          shiny::modalDialog(
            shiny::div(
              tags$style(HTML(".modal-dialog {width: 90%; max-width: 90%;}")),
              reactable::renderReactable({
                .renderTable(df_lasso)
              }),
              size = "l",
              easyClose = FALSE,
              title = paste0("Entries (", nrow(df_lasso), ")"),
              footer = shiny::modalButton("Close"),
              options = list(
                autowidth = TRUE
              )
            ))
        )
        r$line_to_plot <- NULL
        r$force_update <- !r$force_update
      } else {
        # single point selected, either by click or marquee
        # browser()
        selected_rows_clean <- stringr::str_remove_all(selected_rows, "@.*")
        line_to_plot <- gg_data |>
          dplyr::filter(code %in% selected_rows_clean) |>
          dplyr::arrange(code, time_period) |>
          dplyr::mutate(position = match(time_period, unique(as.character(gg_data$time_period)))) |>
          dplyr::mutate(name = ifelse(!is.na(position), paste0("panel-1-", position), "NA"))  |>
          dplyr::select(code, domain, name, cases_per, controls_per, data_id)
        # update reactive values
        r$line_to_plot <- line_to_plot
      }

    }, ignoreInit = TRUE)

    #
    # render the table ####
    #
    .renderTable <- function(df){
      df <- df |>
        dplyr::mutate(GROUP = stringr::str_replace(GROUP, stringr::fixed("from "), "")) |>
        dplyr::mutate(code = round(code/1000)) |>
        dplyr::mutate(meanCases = round(meanCases, 3)) |>
        dplyr::mutate(meanControls = round(meanControls, 3))|>
        dplyr::mutate(sdCases = round(sdCases, 3)) |>
        dplyr::mutate(sdControls = round(sdControls, 3))|>
        dplyr::mutate(beta = round(log(OR), 3)) |>
        dplyr::mutate(pLog10 = round(-log10(p), 3)) |>
        dplyr::mutate(OR = dplyr::case_when(
          OR > 10e+100 ~ Inf,
          OR < 10e-100 ~ -Inf,
          TRUE ~ round(OR, 3)
        )) |>
        dplyr::select(
          GROUP, name, conceptCode, vocabularyId, code, analysisName, domain, upIn,
          nCasesYes, nControlsYes, meanCases, meanControls, sdCases, sdControls,
          OR, pLog10, beta, notes)

      reactable::reactable(
        df,
        filterable = TRUE,
        bordered = TRUE,
        # highlight = TRUE,
        striped = TRUE,
        defaultColDef = reactable::colDef(
          resizable = TRUE
        ),
        defaultSorted = list(pLog10 = "desc", OR = "desc"),
        sortable = TRUE,
        columns = list(
          GROUP = reactable::colDef(name = "Time ID", minWidth = 20, align = "right"),
          name = reactable::colDef(
            name = "Covariate Name",
            cell = function(name, rowIndex) {
              htmltools::tags$a(
                href = paste0(atlasUrl, "/#/concept/", df$code[ rowIndex ]),
                target = "_blank", df$name[ rowIndex ],
                content = name
              )
            },
            minWidth = 50
          ),
          code = reactable::colDef(show = FALSE),
          conceptCode = reactable::colDef(name = "Concept Code", minWidth = 15),
          vocabularyId = reactable::colDef(name = "Vocabulary", minWidth = 15),
          analysisName = reactable::colDef(name = "Analysis Name", minWidth = 50),
          domain = reactable::colDef(name = "Domain", minWidth = 40),
          upIn = reactable::colDef(name = "Type", minWidth = 15),
          nCasesYes = reactable::colDef(name = "N cases", minWidth = 12),
          nControlsYes = reactable::colDef(name = "N ctrls", minWidth = 12),
          meanCases = reactable::colDef(name = "Ratio|Mean cases", minWidth = 13),
          meanControls = reactable::colDef(name = "Ratio|Mean ctrls", minWidth = 13),
          sdCases = reactable::colDef(name = "SD cases", minWidth = 13),
          sdControls = reactable::colDef(name = "SD ctrls", minWidth = 13),
          OR = reactable::colDef( name = "OR", minWidth = 25),
          pLog10 = reactable::colDef(name = "mlogp", minWidth = 25),
          beta = reactable::colDef(name = "Beta", minWidth = 25),
          notes = reactable::colDef(name = "Notes", minWidth = 30)
        ),
        searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE
      )
    }

    #
    # Table View (as reactable) ####
    #
    output$reactableData <- reactable::renderReactable({
      shiny::req(r$filteredTimeCodeWASData)

      .renderTable(r$filteredTimeCodeWASData)
    })

    #
    # download filtered data as a table
    #
    output$downloadDataFiltered <- shiny::downloadHandler(
      filename = function(){
        paste('timecodewas_filtered_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.csv', sep='')
      },
      content = function(fname){
        readr::write_csv(r$filteredTimeCodeWASData |> dplyr::select(-label), fname)
        return(fname)
      }
    )

    #
    # download all data as a table
    #
    output$downloadDataAll <- shiny::downloadHandler(
      filename = function(){
        paste('timecodewas_all_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.csv', sep='')
      },
      content = function(fname){
        readr::write_csv(r$timeCodeWASData |> dplyr::select(-label), fname)
        return(fname)
      }
    )

    #
    # download data as a plot
    #
    output$downloadProportionsView <- shiny::downloadHandler(
      filename = function(){
        paste('timecodewas_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.pdf', sep='')
      },
      content = function(fname){

        grDevices::cairo_pdf(filename = fname,
                             width = 15,
                             height = 5,
                             pointsize = 20,
                             family = "sans",
                             bg = "transparent",
                             antialias = "default",
                             fallback_resolution = 300,
        )
        print(r$savedProportionsView)
        grDevices::dev.off()
      },
      contentType = "application/pdf"
    )

    #
    # download data as a plot (Plot2) ####
    #
    output$downloadProgressView <- shiny::downloadHandler(
      filename = function(){
        paste('timecodewas_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.pdf', sep='')
      },
      content = function(fname){

        grDevices::cairo_pdf(filename = fname,
                             width = 15,
                             height = 6,
                             pointsize = 20,
                             family = "sans",
                             bg = "transparent",
                             antialias = "default",
                             fallback_resolution = 300,
        )
        print(r$savedProgressView)
        grDevices::dev.off()
      },
      contentType = "application/pdf"
    )

    #
    # Progress View ####
    #
    output$progressView <- ggiraph::renderGirafe({
      shiny::req(r$filteredTimeCodeWASData)

      top_colors <- c(
        "1"= "blue",
        "2" = "magenta",
        "3" = "green",
        "4" = "orange",
        "5" = "turquoise",
        "6" = "purple",
        "7" = "darkgreen",
        "8" = "cyan",
        "9" = "brown",
        "10" = "pink",
        "11" = "lightgray"
      )

      # get the unique time periods, effectively dropping the unused ones
      timePeriods <- r$filteredTimeCodeWASData$time_period |> unique()

      gg_data <- r$filteredTimeCodeWASData |>
        dplyr::mutate(time_period = factor(time_period, levels = timePeriods, labels = timePeriods)) |>
        dplyr::mutate(name = factor(name, levels = unique(name))) |>
        dplyr::mutate(name = stringr::str_remove_all(name, "'")) |>
        dplyr::mutate(label = stringr::str_remove_all(label, "'")) |>
        dplyr::mutate(oddsRatio = ifelse(is.na(OR), 1, OR)) |>
        dplyr::mutate(pLog10 = -log10(p)) |>
        dplyr::mutate(pLog10 = ifelse(pLog10 < 1e-30, 1e-30, pLog10)) |>
        dplyr::mutate(beta = log(oddsRatio)) |>
        dplyr::mutate(beta = ifelse(beta > 5, 5, beta)) |>
        dplyr::mutate(beta = ifelse(beta < -5, -5, beta)) |>
        dplyr::mutate(direction = ifelse(beta > 0, "cases", "controls")) |>
        dplyr::mutate(data_id = paste0(name,analysisName)) |>
        dplyr::mutate(time_period_jittered = jitter(as.numeric(time_period), amount = 0.3)) |>
        dplyr::mutate(pLog10_jittered = jitter(pLog10, amount = 0.1 * max(pLog10)))

      items <- gg_data |>
        dplyr::select(name, analysisName, pLog10) |>
        dplyr::arrange(desc(pLog10)) |>
        dplyr::mutate(rank = row_number()) |>
        dplyr::group_by(name) |>
        dplyr::mutate(rank = min(rank, na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::distinct(name, analysisName, rank) |>
        dplyr::arrange(rank) |>
        dplyr::mutate(rank = seq_along(rank)) |>
        dplyr::mutate(color_group = ifelse(rank <= input$label_limit, as.character(rank), "11")) |>
        dplyr::select(name, analysisName, color_group, rank)

      if(nrow(gg_data) == 0){
        return(NULL)
      }

      gg_data <- gg_data |>
        left_join(items, by = c("name", "analysisName")) |> dplyr::arrange(desc(rank))

      gg_plot <- ggplot2::ggplot(gg_data, ggplot2::aes(x = time_period_jittered, y = pLog10_jittered,  group = data_id, fill = color_group, color = color_group)) +
        ggiraph::geom_point_interactive(
          data = gg_data |> dplyr::filter(color_group == "11"),
          aes(size = pLog10, data_id = data_id, tooltip = label),
          color = "black", shape = 21, alpha = 0.2
        ) +
        {if(input$connect_dots)
          ggplot2::geom_line(data = gg_data |> dplyr::filter(color_group != "11"), linewidth = 1)
        } +
        {if(input$show_labels)
          ggrepel::geom_text_repel(
            data = gg_data |> dplyr::filter(color_group != "11"),
            ggplot2::aes(label = stringr::str_trunc(name, 36)), # limit label width
            color = "black",
            segment.color = "black",
            segment.size = 0.3,
            segment.linetype = "dashed",
            max.overlaps = Inf,
            size = 3,
            hjust = 0.1,
            force = 0.5,
            force_pull = 0.5,
            # xlim = c(0.5, NA),
            box.padding = 0.8
          )
        } +
        ggiraph::geom_point_interactive(
          data = gg_data |> dplyr::filter(color_group != "11"),
          aes(size = pLog10, data_id = data_id, tooltip = label),
          color = "black", shape = 21, alpha = 1
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "bottom",
          # plot.title = ggplot2::element_text(size = 16, margin = ggplot2::margin(t = 10, b = 5)),
          axis.text.x = ggplot2::element_text(size = 10),
          axis.text.y = ggplot2::element_text(size = 10),
          axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), size = 10),
          axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 15), size = 10),
        ) +
        ggplot2::scale_color_manual(name = "color_group", values = top_colors) +
        ggplot2::scale_fill_manual(name = "color_group", values = top_colors) +
        ggplot2::scale_x_continuous(breaks = c(1:length(levels(gg_data$time_period))), labels = levels(gg_data$time_period)) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.35, 0.35))) +
        ggplot2::labs(
          x = "Time period",
          y = "-log10(p)",
          title = "",
          subtitle = "",
          caption = lubridate::today()
        ) +
        ggplot2::guides(
          color = "none",
          fill = "none",
          size = "none"
        )

      r$savedProgressView <- gg_plot

      gg_girafe <- ggiraph::girafe(ggobj = gg_plot, height_svg = 7, width_svg = 14) # plot size in inches
      gg_girafe <- ggiraph::girafe_options(
        gg_girafe,
        ggiraph::opts_sizing(rescale = TRUE, width = 1.0),
        ggiraph::opts_hover(
          css = "fill-opacity:1;fill:red;stroke:black;",
          reactive = FALSE
        ),
        ggiraph::opts_selection(
          type = c("none"),
          only_shiny = TRUE
        ),
        ggiraph::opts_toolbar(
          position = "topright",
          hidden = c("zoom", "zoomReset", "lasso_select", "lasso_deselect", "saveaspng"),
          delay_mouseout = 100000
        )
      )

      return(gg_girafe)
    }) # renderPlot

    #
    # utility functions
    #

    .label_editor <- function(s){
      for(i in 1:length(s)){
        s[[i]] <- .label_editor_single(s[[i]])
      }
      return(s)
    }

    .label_editor_single <- function(s){
      return(s)
    }

    #
    # Convert days to years and months, e.g. "1y 3m"
    #

    .vectorized_convert_days <- Vectorize(function(d) {
      if (is.na(d)) return(NA_character_)
      is_negative <- d < 0
      abs_days <- abs(d)
      start_date <- lubridate::ymd("1900-01-01")
      end_date <- start_date + lubridate::days(abs_days)
      diff <- lubridate::as.period(lubridate::interval(start_date, end_date))
      years <- diff@year
      months <- diff@month
      # Format the result
      result <- case_when(
        years == 0 & months == 0 ~ paste0("0"),
        years == 0 & months != 0 ~ paste0(months, "m"),
        years != 0 & months == 0 ~ paste0(years, "y"),
        TRUE ~ paste0(years, "y ", months, "m")
      )
      if (is_negative) {
        result <- paste0("-", result)
      }
      return(result)
    })

    .get_time_periods <- function(studyResults){
      timeRange <- studyResults |>
        dplyr::select(startDay, endDay) |>
        dplyr::distinct() |>
        dplyr::collect() |>
        na.omit() |>
        dplyr::arrange(startDay, endDay) |>
        dplyr::mutate(startDayText = .vectorized_convert_days(as.integer(startDay))) |>
        dplyr::mutate(endDayText = .vectorized_convert_days(as.integer(endDay))) |>
        dplyr::mutate(timeRange = paste0(startDayText, " / ", endDayText))

      return(timeRange)
    }

  }) # shinyServer
} # mod_timeCodeWASPlot_server












