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
mod_resultsVisualisation_CohortsDemographics_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$h4("Filters"),
    shiny::uiOutput(ns("CDPlot_ui")),
    shiny::tags$h4("Data"),
    shiny::tabsetPanel(
      id = ns("tabset"),
      shiny::tabPanel(
        "Plot",
        shiny::plotOutput(ns("demographicsPlot"), height = "600px"),
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
      )
    ),
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
mod_resultsVisualisation_CohortsDemographics_server <- function(id, analysisResults) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cohortDemographicsData <- shiny::reactive({
      analysisResults |> dplyr::tbl('demographicsCounts') |>
        dplyr::collect()
    })

    # stores the last plot for download
    last_plot <- NULL

    #
    # data to be plotted
    #
    ggplotData <- shiny::reactive({
      shiny::req(cohortDemographicsData())
      shiny::req(input$stratifyBy)
      # shiny::req(input$databaseId)
      shiny::req(input$cohortId)
      shiny::req(input$gender)
      shiny::req(input$referenceYear)

      # add cohortId to grouping_vars
      grouping_vars <- c(input$stratifyBy, "cohortId")

      cohortDemographicsData() |>
        dplyr::mutate(
          ageGroup = forcats::fct_reorder(
            ageGroup, as.numeric(stringr::str_extract(ageGroup, "\\d+")))
        ) |>
        # filters
        # dplyr::filter(databaseId %in% input$databaseId) |>
        dplyr::filter(cohortId %in% input$cohortId) |>
        dplyr::filter(gender %in% input$gender) |>
        dplyr::filter(referenceYear == input$referenceYear) |>
        dplyr::group_by(across(grouping_vars)) |>
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
        dplyr::distinct(cohortId) |>
        dplyr::pull(cohortId)


      shiny::fluidPage(
        column(
          3,
          shiny::tagList(
            # shinyWidgets::pickerInput(
            #   ns("databaseId"), "Select database",
            #   choices = unique(cdd$databaseId), unique(cdd$databaseId),
            #   selected = dplyr::first(unique(cdd$databaseId)), multiple = FALSE
            # ),
            shinyWidgets::pickerInput(
              ns("cohortId"), "Select cohorts", choices = cohorts_in_database, selected = cohorts_in_database, multiple = TRUE),
            shinyWidgets::pickerInput(
              ns("referenceYear"), "Show patient counts for", choices = unique(cdd$referenceYear), selected = "cohort_start_date", multiple = FALSE),
          ),
        ),
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
        ),
        column(
          3,
          shiny::tagList(
            shiny::div(
              shiny::checkboxInput(ns("show_count"), "Show patient counts", value = FALSE),
              style="margin-top: 22px; margin-bottom: -15px;"
            ),
            shiny::div(
              shiny::checkboxInput(ns("same_scale"), "Use same y-scale across cohorts", value = TRUE),
              style="margin-top: -15px; margin-bottom: -15px;"
            ),
          )
        ),
      )

    })

    # #
    # # Update cohortId picker based on databaseId
    # #
    # shiny::observeEvent(input$databaseId, {
    #   shiny::req(cohortDemographicsData())
    #   shiny::req(input$databaseId)
    #
    #   cohorts_in_database <- cohortDemographicsData() |>
    #     dplyr::filter(databaseId %in% input$databaseId) |>
    #     dplyr::distinct(cohortId) |>
    #     dplyr::pull(cohortId)
    #
    #   shinyWidgets::updatePickerInput(session, "cohortId", choices = cohorts_in_database, selected = cohorts_in_database)
    # }, ignoreInit = FALSE)

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
        ggplot2::geom_col(position = "dodge", width = 0.8) +
        {if(input$show_count)
          ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.7, position = ggplot2::position_dodge(width = .8))
        } +
        ggplot2::facet_grid(eval(ggplot2::expr(!!ggplot2::ensym(rows) ~ !!ggplot2::ensym(cols))), scales = ifelse(input$same_scale, "fixed", "free_y")) +
        {if(x == "calendarYear")
          ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1)))))
        } +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::expand_limits(y = max(ggplotData()$count) * 1.1) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 12, angle = text_angle, hjust = 1),
          strip.background = ggplot2::element_rect(fill="white"),
          strip.text = ggplot2::element_text(colour = 'black')
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

      if(identical(c("ageGroup", "gender", "calendarYear"), input$stratifyBy)){
        gg_plot <- build_plot(
          x = "calendarYear", y = "count", fill = "gender",
          rows = "cohortId", cols = "ageGroup",
          title = "Cohort Demographics", x_label = "Calendar time by age group", y_label = "Count")
      } else if(identical(c("ageGroup", "gender"), input$stratifyBy)) {
        gg_plot <- build_plot(
          x = "ageGroup", y = "count", fill = "gender",
          rows = "cohortId", cols = ".",
          title = "Cohort Demographics", x_label = "Age group", y_label = "Count")
      } else if(identical(c("ageGroup", "calendarYear"), input$stratifyBy)) {
        gg_plot <- build_plot(
          x = "calendarYear", y = "count", fill = "",
          rows = "cohortId", cols = "ageGroup",
          title = "Cohort Demographics", x_label = "Calendar time by age group", y_label = "Count")
      } else if(identical(c("gender", "calendarYear"), input$stratifyBy)) {
        gg_plot <- build_plot(
          x = "calendarYear", y = "count", fill = "gender",
          rows = "cohortId", cols = "gender",
          title = "Cohort Demographics", x_label = "Calendar time by gender", y_label = "Count")
      } else if(identical(c("ageGroup"), input$stratifyBy)) {
        gg_plot <- build_plot(
          x = "ageGroup", y = "count", fill = "",
          rows = "cohortId", cols = ".",
          title = "Cohort Demographics", x_label = "Age group", y_label = "Count")
      } else if(identical(c("gender"), input$stratifyBy)) {
        gg_plot <- build_plot(
          x = "gender", y = "count", fill = "gender",
          rows = "cohortId", cols = ".",
          title = "Cohort Demographics", x_label = "Gender", y_label = "Count")
      } else if(identical(c("calendarYear"), input$stratifyBy)) {
        gg_plot <- build_plot(
          x = "calendarYear", y = "count", fill = "",
          rows = "cohortId", cols = ".",
          title = "Cohort Demographics", x_label = "Calendar time", y_label = "Count")
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
      reactable::reactable(ggplotData())
    })

    #
    # download demographics table
    #
    output$downloadDataActionButton <- shiny::downloadHandler(
      filename = function(){
        paste(format(lubridate::now(), "%Y_%m_%d_%H%M"), '_demographics_data.csv', sep='')
      },
      content = function(fname){
        readr::write_csv(ggplotData(), fname)
        return(fname)
      }
    )

    #
    # download demographics table
    #
    output$downloadPlotButton <- shiny::downloadHandler(
      filename = function(){
        paste(format(lubridate::now(), "%Y_%m_%d_%H%M"), '_demographics_plot.pdf', sep='')
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


