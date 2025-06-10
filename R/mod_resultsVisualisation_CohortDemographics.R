#' @title Cohort Demographics Visualization UI
#' @description UI module for visualizing cohort demographics. This module provides controls to customize the appearance of the plot and options to download the plot and data.
#'
#' @param id A string representing the module's namespace.
#'
#' @return A Shiny UI element that can be included in a Shiny app.
#'
#' @importFrom shiny NS tagList tags h4 uiOutput tabsetPanel tabPanel plotOutput div downloadButton
#' @importFrom reactable reactableOutput
#'
#' @export
#'
mod_resultsVisualisation_CohortsDemographics_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::tagList(
      shiny::tagList(
        tags$head(
          tags$style(HTML("
         html, body {
           height: 100%;
           margin: 0;
           padding: 0;
         }
         #main-container {
            height: calc(100vh - 100px);
            border: 0px solid #888;
            padding: 0px;
            box-sizing: border-box;
            overflow: hidden;
          }

         .menu-section {
           margin-bottom: 10px;
           border: 1px solid #ccc;
           border-radius: 4px;
           width: 100%;
         }

         .collapsible-header {
           cursor: pointer;
           align-items: center;
           font-weight: normal;
           margin-top: 0px;
          box-sizing: border-box;
           padding: 0px;
           background-color: #f8f9fa;
         }

         .triangle {
           display: inline-block;
           margin-right: 10px;
           transition: transform 0.3s ease;
         }

         .rotate {
           transform: rotate(90deg);
         }

         .collapsible-content {
           display: none;
           padding: 10px;
           // border-left: 1px solid #ccc;
           background-color: #f8f9fa;
         }
      ")),
          tags$script(HTML(paste0("
           const inputId = '", ns("free_space"), "';

           function sendFreeSpace(containerId) {

             const totalHeight = window.innerHeight;

             Shiny.setInputValue(inputId, {
               total: totalHeight,
               nonce: Math.random()
             }, {priority: 'event'});
           }

           function toggleSection(header) {
             const triangle = header.querySelector('.triangle');
             const content = header.nextElementSibling;

             triangle.classList.toggle('rotate');

             if (content.style.display === 'block') {
               content.style.display = 'none';
             } else {
               content.style.display = 'block';
             }
           }

           window.addEventListener('resize', () => {
             sendFreeSpace('main-container');
           });

           document.addEventListener('DOMContentLoaded', () => {
             setTimeout(() => {
               sendFreeSpace('main-container');
             }, 500);
           });

           Shiny.addCustomMessageHandler('sendFreeSpace', function(message) {
             sendFreeSpace('main-container');
           });

          const rows_to_show_id = '", ns("rows_to_show"), "';

          //
          // Send the number of rows to Shiny when the app loads or resizes
          //
          function updateTableRows() {
              const rowHeight = 36.3; // Approximate row height in px
              const padding = 320;  // Space for header/footer/other elements
              const availableHeight = window.innerHeight - padding;
              const rowCount = Math.floor(availableHeight / rowHeight);
              Shiny.setInputValue(rows_to_show_id, rowCount, {priority: 'event'});
            }
          $(document).on('shiny:connected', updateTableRows);
          $(window).on('resize', updateTableRows);
          ") # end of paste0
          ) # end of HTML
          )# end of tags$script
        ), # end of tags$head
        shiny::div(
          id = "main-container",
          shiny::uiOutput(ns("CDPlot_ui")),
          # shiny::tags$h4("Data"),
          shiny::tabsetPanel(
            id = ns("tabset"),
            shiny::tabPanel(
              "Plot",
              shiny::tagList(
                shiny::div(
                  tags$div(
                    style = "display: flex; align-items: left; gap: 5px;",  # CSS to align and add spacing
                    shiny::checkboxInput(ns("show_count"), "Show patient counts", value = FALSE),
                    shiny::checkboxInput(ns("same_scale"), "Use same y-scale across cohorts", value = TRUE),
                  ),
                )
              ), # end of tagList
              shiny::div(
                style = "height: calc(100vh - 350px);",
                shiny::plotOutput(ns("demographicsPlot"), height = "100%")
              ),
              shiny::div(
                style = "margin-top: 10px; margin-bottom: 10px;",
                shiny::downloadButton(ns("downloadPlotButton"), "Download")
              )
            ),
            shiny::tabPanel(
              "Table",
              reactable::reactableOutput(ns("demographicsData")),
              shiny::div(
                style = "margin-top: 10px; margin-bottom: 10px;",
                shiny::downloadButton(ns("downloadDataActionButton"), "Download")
              )
            ) # end of tabPanel
          ) # end of tabsetPanel
        ) # end of div#main-container
      )
  ))
}


#' @title Cohort Demographics Visualization Server
#' @description Server module for handling the logic of the cohort demographics visualization UI. This module creates a demographics plot based on the analysis results and allows the plot and data to be downloaded.
#'
#' @param id A string representing the module's namespace.
#' @param analysisResults Pooled connection to the analysis results duckdb.
#'
#' @return The module returns server-side logic to generate and manage the cohort demographics plot.
#'
#' @importFrom shiny moduleServer reactive req renderPlot downloadHandler renderUI isTruthy
#' @importFrom shinyjs toggle
#' @importFrom dplyr tbl collect mutate inner_join select filter group_by summarise ungroup across
#' @importFrom forcats fct_reorder
#' @importFrom stringr str_extract
#' @importFrom ggplot2 sym aes geom_col position_dodge2 geom_text facet_grid scale_x_continuous coord_cartesian expand_limits theme_minimal element_text element_rect labs ggplot
#' @importFrom reactable renderReactable reactable
#' @importFrom readr write_csv
#' @importFrom lubridate now
#' @importFrom grDevices cairo_pdf dev.off
#'
#' @export
#'
mod_resultsVisualisation_CohortsDemographics_server <- function(id, analysisResults) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cohortDemographicsData <- shiny::reactive({
      dplyr::inner_join(
        analysisResults |> dplyr::tbl('demographicsCounts') |>
          dplyr::collect(),
        analysisResults |> dplyr::tbl('cohortsInfo') |>
          dplyr::collect() |>
          dplyr::select(cohortId, shortName)
        , by = "cohortId")
    })

    # stores the last plot for download
    last_plot <- NULL

    rows_to_show_debounced <- shiny::debounce(shiny::reactive(input$rows_to_show), 500)

    #
    # data to be plotted
    #
    ggplotData <- shiny::reactive({
      shiny::req(cohortDemographicsData())
      shiny::req(input$stratifyBy)
      # shiny::req(input$databaseId)
      shiny::req(input$shortName)
      shiny::req(input$gender)
      shiny::req(input$referenceYear)

      # add shortName to grouping_vars
      grouping_vars <- c(input$stratifyBy, "shortName")

      cohortDemographicsData() |>
        dplyr::mutate(
          ageGroup = forcats::fct_reorder(
            ageGroup, as.numeric(stringr::str_extract(ageGroup, "\\d+")))
        ) |>
        dplyr::filter(shortName %in% input$shortName) |>
        dplyr::filter(gender %in% input$gender) |>
        dplyr::filter(referenceYear == input$referenceYear) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
        dplyr::summarise(count = sum(count)) |>
        dplyr::ungroup()
    })

    #
    # UI for the cohort demographics plot
    #
    output$CDPlot_ui <- shiny::renderUI({
      shiny::req(cohortDemographicsData())

      cdd <- cohortDemographicsData()

      cohorts_in_database <- cdd |>
        dplyr::distinct(shortName) |>
        dplyr::pull(shortName)

      ui <- shiny::tagList(
        div(class = "menu-section",
            div(class = "collapsible-header",
                onclick = "toggleSection(this)",
                tags$span(class = "triangle", "\u25B6"),  # ▶
                style = "font-size: 16px; font-weight: normal; padding: 10px;",
                "Filters"
            ),
            div(class = "collapsible-content",
                shiny::fluidRow(
                column(
                  3,
                  shiny::tagList(
                    shinyWidgets::pickerInput(
                      ns("shortName"), "Select cohorts", choices = cohorts_in_database, selected = cohorts_in_database, multiple = TRUE),
                    shinyWidgets::pickerInput(
                      ns("referenceYear"), "Show patient counts for", choices = unique(cdd$referenceYear), selected = "cohort_start_date", multiple = FALSE),
                  ),
                ), # end of column
                column(
                  3,
                  shiny::tagList(
                    shinyWidgets::pickerInput(
                      ns("gender"), "Gender", choices = unique(cdd$gender), selected = unique(cdd$gender), multiple = TRUE),
                  ),
                  shinyWidgets::pickerInput(
                    ns("stratifyBy"), "Stratify by",
                    choices = c("ageGroup", "gender", "calendarYear"),
                    selected = c("ageGroup", "gender", "calendarYear"), multiple = TRUE),
                ) # end of column
                ) # end of fluidRow
            ) # end of div.collapsible-content
        ) # end of div.menu-section
      ) # end of tagList

      session$onFlushed(function() {
        session$sendCustomMessage("setupCollapsiblesAgain", list())
      }, once = TRUE)

      ui

    })

    #
    # helper function to build the plot
    #
    build_plot <- function(x, y, fill, rows, cols, title, x_label, y_label){
      x <- ggplot2::sym(x)
      y <- ggplot2::sym(y)
      fill <- if(fill != "") ggplot2::sym(fill)
      text_angle <- if(x == "calendarYear") 45 else 0
      gg_plot <- ggplotData() |>
        ggplot2::ggplot(ggplot2::aes(x = !!x, y = !!y, fill = !!fill)) +
        ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single"), width = 0.8) +
        {if(input$show_count)
          ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.7, position = ggplot2::position_dodge(width = .8))
        } +
        {if(input$same_scale)
          ggplot2::facet_grid(eval(ggplot2::expr(!!ggplot2::ensym(rows) ~ !!ggplot2::ensym(cols))))
        } +
        {if(!input$same_scale)
          ggh4x::facet_grid2(eval(ggplot2::expr(!!ggplot2::ensym(rows) ~ !!ggplot2::ensym(cols))), scales = "free_y", independent = "y")
        } +
        {if(x == "calendarYear")
          ggplot2::scale_x_continuous(expand = c(0.05, 0.05))
        } +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 12, angle = text_angle, hjust = 0.5),
          strip.background = ggplot2::element_rect(fill="white"),
          strip.text = ggplot2::element_text(colour = 'black', size = 12)
        ) +
        {if("gender" %in% input$stratifyBy)
          ggplot2::scale_fill_manual(values = c("Female" = "#BF616A", "Male" = "#2c5e77", "Other" = "#8C8C8C"))
        } +
        ggplot2::labs(title = title, x = x_label, y = y_label)
      return(gg_plot)
    }

    #
    # Plot demographics
    #
    output$demographicsPlot <- shiny::renderPlot({
      shiny::req(shiny::isTruthy(cohortDemographicsData()))
      shiny::req(ggplotData())

      # ggplot absolutely requires data to be present
      if(nrow(ggplotData()) == 0) return(NULL)

      plot_title <- ""

      if(identical(c("ageGroup", "gender", "calendarYear"), input$stratifyBy)){
        gg_plot <- build_plot(
          x = "calendarYear", y = "count", fill = "gender",
          rows = "shortName", cols = "ageGroup",
          title = plot_title, x_label = "Calendar time by age group", y_label = "Count")
      } else if(identical(c("ageGroup", "gender"), input$stratifyBy)) {
        gg_plot <- build_plot(
          x = "ageGroup", y = "count", fill = "gender",
          rows = "shortName", cols = ".",
          title = plot_title, x_label = "Age group", y_label = "Count")
      } else if(identical(c("ageGroup", "calendarYear"), input$stratifyBy)) {
        gg_plot <- build_plot(
          x = "calendarYear", y = "count", fill = "",
          rows = "shortName", cols = "ageGroup",
          title = plot_title, x_label = "Calendar time by age group", y_label = "Count")
      } else if(identical(c("gender", "calendarYear"), input$stratifyBy)) {
        gg_plot <- build_plot(
          x = "calendarYear", y = "count", fill = "gender",
          rows = "shortName", cols = "gender",
          title = plot_title, x_label = "Calendar time by gender", y_label = "Count")
      } else if(identical(c("ageGroup"), input$stratifyBy)) {
        gg_plot <- build_plot(
          x = "ageGroup", y = "count", fill = "",
          rows = "shortName", cols = ".",
          title = plot_title, x_label = "Age group", y_label = "Count")
      } else if(identical(c("gender"), input$stratifyBy)) {
        gg_plot <- build_plot(
          x = "gender", y = "count", fill = "gender",
          rows = "shortName", cols = ".",
          title = plot_title, x_label = "Gender", y_label = "Count")
      } else if(identical(c("calendarYear"), input$stratifyBy)) {
        gg_plot <- build_plot(
          x = "calendarYear", y = "count", fill = "",
          rows = "shortName", cols = ".",
          title = plot_title, x_label = "Calendar time", y_label = "Count")
      } else {
        return(NULL)
      }

      # save the last plot for download
      last_plot <<- gg_plot

      return(gg_plot)
    })

    #
    # show demographics data as a table
    #
    output$demographicsData <- reactable::renderReactable({
      reactable::reactable(
        ggplotData(),
        defaultPageSize = rows_to_show_debounced()
      )
    })

    #
    # download demographics table ####
    #
    output$downloadDataActionButton <- shiny::downloadHandler(
      filename = function(){
        paste('demographics_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.csv', sep='')
      },
      content = function(fname){
        readr::write_csv(ggplotData(), fname)
        return(fname)
      }
    )

    #
    # download demographics plot ####
    #
    output$downloadPlotButton <- shiny::downloadHandler(
      filename = function(){
        paste('demographics_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.pdf', sep='')
      },
      content = function(fname){

        grDevices::cairo_pdf(filename = fname,
                             width = 15,
                             height = 10,
                             pointsize = 1.0,
                             family = "sans",
                             bg = "transparent",
                             antialias = "default",
                             fallback_resolution = 300,
        )
        print(last_plot)
        grDevices::dev.off()
      },
      contentType = "application/pdf"
    )


  })
}


