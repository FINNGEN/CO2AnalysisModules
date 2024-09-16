

mod_fct_formTimeWindows_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    shiny::tags$div(
      style = "margin-left: 30px; margin-right: 50px; min-width: 600px;",
      shiny::br(),
      shiny::fixedRow(
        style = "margin-bottom: 130px;",
        shiny::column(
          3, offset = 0, style="margin-top:0px; ",
          shiny::textInput(
            ns("window_breakpoints"), "Window breakpoints", value = "")
        ),
        shiny::column(
          3, offset = 0, style="margin-left:10px;margin-top: 0px;",
          shiny::selectInput(
            inputId = ns("presets"),
            label = "Presets",
            multiple = FALSE,
            choices = c(
              "-5, -1, 1, 5",
              "-10, -5, -1, 1, 5, 10",
              "-20, -10, -2, 2, 10, 20",
              "-30, 30, 90, 180, 365"
            ),
            selected = "",
            selectize = TRUE
          )
        ),
        shiny::column(
          2, offset = 0,  style="margin-top:0px;",
          shiny::selectInput(
            inputId = ns("window_type"),
            label = "Windows as",
            choices = c("months", "years"),
            selected = "years",
          )
        ), # column
        shiny::column(
          2, offset = 0,  style="margin-top:25px;",
          shinyWidgets::awesomeCheckbox(
            inputId = ns("zero_window"),
            label = "Add [0,0] window",
            status = "primary",
            value = TRUE
          )
        ), # column
      ),
      shiny::uiOutput(ns("slider.ui")),
      shiny::fixedRow(
        shiny::column(
          3, offset = 0,
          shiny::tags$h5("Breakpoints"),
          shiny::tableOutput(ns("slider_value"))
        ),
        shiny::column(
          3, offset = 3,
          shiny::tags$h5("Window sizes"),
          shiny::tableOutput(ns("slider_distance"))
        )
      ),
      shiny::fixedRow(
        shiny::column(12, offset = 0, style = "margin-top: 10px;",
                      shiny::tags$h5("Time windows (as days)"),
                      shiny::verbatimTextOutput(ns("the_window_output")))
      ),
      shiny::br(),
      shiny::actionButton(ns("slider_help_button"), "Help"),
      shinyjs::hidden(
        shiny::verbatimTextOutput(ns("slider_help"))
      )
    )
  )
}


mod_fct_formTimeWindows_server <- function(id, session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    years_before <- shiny::reactiveVal(-5)
    years_after <- shiny::reactiveVal(5)
    windows <- shiny::reactiveVal(4)
    time_windows <- shiny::reactiveVal(c(-5, -1, 1, 5))

    days_before <- shiny::reactive({
      shiny::req(years_before());
      floor(years_before() * ifelse(input$window_type == "months", 30.42, 365.25))
    })

    days_after <- shiny::reactive({
      shiny::req(years_after());
      ceiling(years_after() * ifelse(input$window_type == "months", 30.42, 365.25))
    })

    distance_table <- function(x, col_names){
      dt <- tibble::tibble(x,
                           round(lubridate::days(x)/lubridate::weeks(1), 1),
                           lubridate::days(x)/months(1),
                           lubridate::days(x)/lubridate::years(1)) |>
        dplyr::mutate(row = dplyr::row_number(), .before = starts_with("x"))
      names(dt) <- col_names
      dt
    }

    shiny::observeEvent(c(days_before, days_after), {
      shiny::req(days_before(), days_after())
      shinyWidgets::updateNoUiSliderInput(
        inputId = ns("the_slider"),
        range = c(days_before(), days_after())
      )
    })

    output$slider_value <- shiny::renderTable({
      shiny::req(input$the_slider)
      distance_table(input$the_slider, c("Break", "Days", "Weeks", "Months", "Years"))
    }, digits = 1)

    output$slider_distance <- shiny::renderTable({
      shiny::req(input$the_slider)
      distance_table(diff(input$the_slider), c("Window", "Days", "Weeks", "Months", "Years"))
    }, digits = 1)

    #
    # slider.ui
    #
    output$slider.ui <- shiny::renderUI({
      shiny::req(windows(), days_before(), days_after())

      div(
        style = "margin-top: -70px;",
        shinyWidgets::noUiSliderInput(
          inputId = ns("the_slider"),
          label = NULL,
          min = days_before(),
          max = days_after(),
          step = 1,
          value = time_windows(),
          format = shinyWidgets::wNumbFormat(decimals = 0, thousand = "", prefix = ""),
          behaviour =  c("none"),
          color = "#a2dbff",
          update_on = "change", # end, change
          width = "100%"
        )
      )
    })

    output$slider_help <- shiny::renderText({
      paste(
        "You can move the slider thumbs by dragging or by using the arrow keys.",
        "The 'Day [0-0] window' is a special case, and is included by default.",
        sep = "\n"
      )
    })

    shiny::observeEvent(input$slider_help_button, {
      shinyjs::toggle(id = "slider_help")
    })

    #
    # update values, years_before, years_after, windows, time_windows
    #
    shiny::observeEvent(c(input$window_breakpoints, input$window_type),{
      shiny::req(input$window_breakpoints)
      if(!stringr::str_detect(input$window_breakpoints, "^[0-9, -]+$")) {
        shinyFeedback::showFeedbackWarning(
          inputId = "window_breakpoints",
          text = "Invalid input: Please use only numbers separated by commas."
        )
      } else if(stringr::str_detect(input$window_breakpoints, "-{2,}")) {
        shinyFeedback::showFeedbackWarning(
          inputId = "window_breakpoints",
          text = "Invalid input: Duplicate minus signs are not allowed."
        )
      }
      else {
        shinyFeedback::hideFeedback("window_breakpoints")
        values <- as.numeric(unlist(strsplit(input$window_breakpoints, ",")))
        values <- values[!is.na(values)] |> unique() |> sort()
        years_before(min(values))
        years_after(max(values))
        windows(length(values) - 1)
        time_windows(round(values * ifelse(input$window_type == "months", 30.42, 365.25)))
      }
    })

    #
    # update the window_breakpoints from the presets
    #
    shiny::observeEvent(input$presets, {
      shiny::req(input$presets)
      shiny::updateTextInput(session, "window_breakpoints", value = input$presets)
    })

    #
    # reactive for the time window values
    #
    output$the_window_output <- renderText({
      shiny::req(input$the_slider)

      breaks <- as.vector(input$the_slider)
      if(input$zero_window && !0 %in% breaks) breaks <- c(0, breaks)
      result <- c()
      for(value in breaks) {
        if(value == 0) result <- c(result, value, value + 1, value - 1, 0)
        if(value < 0) result <- c(result, value, value - 1)
        if(value > 0) result <- c(result, value, value + 1)
      }
      result <- sort(result)[-c(1, length(result))]
      output <- ""
      for(i in seq(1, length(result), 2)) {
        output <- paste(output, paste("[", result[i], ", ", result[i + 1], "]", sep = ""), sep = "  ")
      }
      toString(output)
    })

    #
    # reactive for the time windows
    #
    shiny::reactive({
      shiny::req(input$the_slider)
      breaks <- as.vector(input$the_slider)
      if(input$zero_window && !0 %in% breaks) breaks <- c(0, breaks)
      result <- c()
      for(value in breaks) {
        if(value == 0) result <- c(result, value, value + 1, value - 1, 0)
        if(value < 0) result <- c(result, value, value - 1)
        if(value > 0) result <- c(result, value, value + 1)
      }
      result <- sort(result)[-c(1, length(result))]
      startDays <- c()
      endDays <- c()
      for(i in seq(1, length(result), 2)) {
        startDays <- c(startDays, result[i])
        endDays <- c(endDays, result[i + 1])
      }

      list(temporalStartDays = startDays, temporalEndDays = endDays)
    })


  })
}
