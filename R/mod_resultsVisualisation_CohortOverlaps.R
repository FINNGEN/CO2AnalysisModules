#' @title Cohort Overlaps Visualization UI
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
mod_resultsVisualisation_CohortsOverlaps_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shinybrowser::detect(),
    shiny::fluidPage(
      title = "UpSet plot",
      shinyjs::useShinyjs(),

      shiny::div(
        style = "margin: 10px; width: 100%;",
        shiny::actionButton(ns("upset_controls_button"), "Show/Hide Settings")
      ),

      shinyjs::hidden(
        shiny::div(
          id = ns("upset_controls"),
          shiny::fluidRow(
            shinyWidgets::chooseSliderSkin("Flat"),
            shiny::column(width = 4, align = "left",
                          shiny::div(style = "height: 85px; width: 100%;",
                                     shiny::sliderInput(ns("nsets"), "Number of sets", min = 1, max = 20, value = 8, step = 1),
                          ),
                          shiny::div(style = "height: 85px; width: 100%;",
                              shiny::sliderInput(ns("nintersects"), "Number of intersections", min = 1, max = 60, value = 40, step = 1),
                          ),
                          shiny::div(style = "height: 85px; width: 100%;",
                              shiny::sliderInput(ns("set_size.scale_max"), "Set size scale max", min = 0.5, max = 2.5, value = 1.3, step = 0.1),
                          ),
            ),
            shiny::column(4, align = "left",
                   shiny::div(style = "height: 85px; width: 100%;",
                       shiny::sliderInput(ns("text_scale"), "Text scale", min = 0.5, max = 3, value = 2.0, step = 0.1),
                   ),
                   shiny::div(style = "height: 85px; width: 100%;",
                       shiny::sliderInput(ns("point_size"), "Point size", min = 1, max = 10, value = 4, step = 1),
                   ),
                   shiny::div(style = "height: 85px; width: 100%;",
                       shiny::sliderInput(ns("number_angles"), "Number angles", min = 0, max = 45, value = 0, step = 1),
                   ),
            ),
            shiny::column(4, align = "left",
                  shiny::div(style = "height: 85px; width: 100%;",
                             shiny::sliderInput(ns("set_size.numbers_size"), "Set size number size", min = 0.5, max = 30, value = 7, step = 0.1),
                   ),
                   shiny::div(style = "height: 85px; width: 100%;",
                       shiny::sliderInput(ns("plot_width"), "Plot width (in)", min = 2, max = 30, value = 18, step = 0.5),
                   ),
                   shiny::div(style = "height: 85px; width: 100%;",
                       shiny::sliderInput(ns("plot_height"), "Plot height (in)", min = 2, max = 30, value = 10, step = 0.5),
                   ),
            ),
          ),
          shiny::fluidRow(
            shiny::column(12, align = "left",
                   shiny::checkboxInput(ns("show_numbers"), "Show intersection sizes", value = TRUE),
                   shiny::checkboxInput(ns("set_size_show"), "Show set sizes", value = TRUE),
            )
          )
        )
      ), # hidden
      shiny::fluidRow(
        shiny::column(12, align = "center",
               shiny::plotOutput(ns("upset_plot")),
               shiny::div(style = "margin-top: 20px;",
                   shiny::downloadButton(ns("download_pdf"), "Download plot as PDF", icon = shiny::icon("download")),
                   shiny::downloadButton(ns("download_csv"), "Download data as CSV", icon = shiny::icon("download")),
               )
        )
      ),
    ),
  )
}


#' @title Cohort Overlaps Visualization Server
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
mod_resultsVisualisation_CohortsOverlaps_server <- function(id, analysisResults) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # cohort overlaps
    #
    cohortOverlaps <- shiny::reactive({
      cohortOverlapsData <- analysisResults |> dplyr::tbl("cohortOverlaps") |> dplyr::collect()
    })

    cohortDefinitionSet <- shiny::reactive({
      cohortDefinitionSetData <- analysisResults |> dplyr::tbl("cohortDefinitionSet") |> dplyr::collect()
    })

    #
    # show/hide the upset plot controls
    #
    shiny::observeEvent(input$upset_controls_button, {
      shinyjs::toggle(id = "upset_controls")
    })

    #
    # render the upset plot
    #
    output$upset_plot <- shiny::renderPlot({

      build_upset_plot()
    })

    #
    # build the upset plot
    #
    build_upset_plot <- shiny::reactive({
      req(cohortOverlaps())

      cohortDefinitionData <- cohortDefinitionSet()
      cohortOverlapsData <- cohortOverlaps()

      for(i in 1:nrow(cohortDefinitionData)){
        cohortOverlapsData$cohortIdCombinations <-
          gsub(paste0("-", cohortDefinitionData$cohortId[i], "-"), paste0("-", cohortDefinitionData$shortName[i], "-"), cohortOverlapsData$cohortIdCombinations)
      }
      cohortOverlapsData$cohortIdCombinations <-gsub("-", "&", cohortOverlapsData$cohortIdCombinations)
      cohortOverlapsData$cohortIdCombinations <- gsub("^&|&$", "", cohortOverlapsData$cohortIdCombinations)

      upset_expr <- with(cohortOverlapsData, setNames(numberOfSubjects, cohortIdCombinations))

      UpSetR::upset(
        UpSetR::fromExpression(upset_expr),
        nsets = input$nsets,
        nintersects = input$nintersects,
        sets.bar.color = "black",
        order.by = "freq",
        keep.order = TRUE,
        text.scale = input$text_scale,
        set_size.show = input$set_size_show,
        point.size = input$point_size,
        show.numbers = ifelse(input$show_numbers, "yes", "no"),
        set_size.numbers_size = input$set_size.numbers_size,
        set_size.scale_max = input$set_size.scale_max * sum(upset_expr) * 1.1, # 10% margin - is there a better way to estimate this?
        number.angles = input$number_angles,
      )
    })

    #
    # download the plot as a PDF file
    #
    output$download_pdf <- downloadHandler(
      filename = function(){
        paste("upset_plot_", Sys.Date(), ".pdf", sep = "")
      },

      content = function(file){
        grDevices::cairo_pdf(filename = file,
                             width = input$plot_width,
                             height = input$plot_height,
                             pointsize = 12,
                             family = "sans",
                             bg = "transparent",
                             antialias = "subpixel",
                             fallback_resolution = 300
        )
        print(build_upset_plot())
        dev.off()
      },

      contentType = "application/pdf"
    )

    #
    # download the data as a CSV file
    #
    output$download_csv <- downloadHandler(
      filename = function(){
        paste0("upset_plot_data_", Sys.Date(), ".csv")
      },

      content = function(file){
        write.csv(cohortOverlaps(), file)
      },

      contentType = "application/csv"
    )


  })
}
