

#' Form Time Windows UI Module
#'
#' This module provides the UI for forming time windows.
#'
#' @param id Module ID
#'
#' @return A Shiny UI element
#'
#' @importFrom shiny NS tagList tags br fixedRow column textInput selectInput uiOutput tableOutput verbatimTextOutput actionButton
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinyWidgets awesomeCheckbox
#'
#' @export
mod_fct_formTimeWindows_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    # custom CSS for the pre tags, #e0f7fa
    shiny::tags$style(HTML("
    .custom-container pre {
      background-color: white !important;
      color: black;
      padding: 10px;
      border-radius: 4px;
      border: 1px solid #004d40;
    }
    ")),
    shiny::tags$div(
      style = "margin-left: 30px; margin-right: 50px; min-width: 600px;",
      shiny::br(),
      shiny::fixedRow(
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
      shiny::fixedRow(
        style = "margin-bottom: 0px;",
        shiny::column(
          3, offset = 0, style="margin-top:0px; ",
          shiny::textInput(
            ns("window_breakpoints"), "Window breakpoints", value = "")
        ),
        shiny::column(
          3, offset = 0, style="margin-left:10px; margin-top: 0px;",
          shiny::selectizeInput(
            inputId = ns("presets"),
            label = "Presets for breakpoints",
            multiple = FALSE,
            choices = c(
              "-5, -1, 1, 5",
              "-10, -5, -1, 1, 5, 10",
              "-20, -10, -5, -2, 2, 5, 10, 20"
             ),
            selected = "",
            options = list(
              placeholder = "Select a preset",
              allowEmptyOption = TRUE
            )
          )
        ),
      ),
      # text outputs with custom containers
      shiny::fixedRow(
        shiny::div(
          shiny::column(12, offset = 0, style = "margin-top: 0px;",
                        shiny::tags$h5("Time windows (approximate)"),
                        shiny::verbatimTextOutput(ns("output_approximate_windows"))),
          class = "custom-container"
        ), # div
      ),
      shiny::fixedRow(
        shiny::div(
          shiny::column(12, offset = 0, style = "margin-top: 0px;",
                        shiny::tags$h5("Time windows (exact, as days)"),
                        shiny::verbatimTextOutput(ns("output_window_as_days"))),
          class = "custom-container"
        ), # div
      )
    ), # div
  )
}

#' Form Time Windows Server Module
#'
#' This module provides the server logic for forming time windows.
#'
#' @param id Module ID
#' @param session Shiny session object
#'
#' @return A reactive list containing the start and end days of the time windows
#'
#' @importFrom shiny moduleServer reactiveVal reactive req renderTable renderUI renderText observeEvent updateTextInput
#' @importFrom shinyjs toggle
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom shinyWidgets noUiSliderInput updateNoUiSliderInput wNumbFormat
#' @importFrom dplyr mutate row_number starts_with
#' @importFrom lubridate days weeks months years
#' @importFrom tibble tibble
#' @importFrom stringr str_detect
#'
#' @export
mod_fct_formTimeWindows_server <- function(id, session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    time_windows <- shiny::reactiveVal(0)

    #
    # update time_windows
    #
    shiny::observeEvent(c(input$window_breakpoints, input$window_type),{
      shiny::req(input$window_breakpoints)
      if(!stringr::str_detect(input$window_breakpoints, "^[0-9, -]+$")) {
        shinyFeedback::showFeedbackWarning(
          inputId = "window_breakpoints",
          text =
            paste0(
              "Invalid input: Please use only positive and negative numbers separated by commas. ",
              "Negative numbers set the time before the event, positive numbers set the time after the event.",
              sep = "\n"
            )
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
        time_windows(round(values * ifelse(input$window_type == "months", 30.44, 365.25)))
      }
    })

    #
    # update the window_breakpoints from the presets
    #
    shiny::observeEvent(input$presets, {
      shiny::req(input$presets)
      shiny::updateTextInput(session, "window_breakpoints", value = input$presets)
      shiny::updateSelectizeInput(session, "presets", selected = "")
    })

    #
    # reactive for the time window values
    #
    output$output_window_as_days <- renderText({
      shiny::req(time_windows())

      breaks <- time_windows()
      if(input$zero_window && !(0 %in% breaks)) breaks <- c(0, breaks)
      result <- c()
      for(value in breaks) {
        if(value == 0) result <- c(result, value, value + 1, value - 1, 0)
        if(value < 0) result <- c(result, value, value - 1)
        if(value > 0) result <- c(result, value, value + 1)
      }
      result <- sort(result)[-c(1, length(result))]
      output <- ""
      for(i in seq(1, length(result), 2)) {
        window_start <- result[i]
        window_end <- result[i + 1]
        output <- paste(output, paste("[", window_start, ", ", window_end, "]", sep = ""), sep = "  ")
      }
      paste0(output, "\n")
    })

    #
    # reactive for the time window values
    #
    output$output_approximate_windows <- renderText({
      shiny::req(time_windows())

      breaks <- time_windows()
      if(input$zero_window && !(0 %in% breaks)) breaks <- c(0, breaks)
      result <- c()
      for(value in breaks) {
        if(value == 0) result <- c(result, value, value + 1, value - 1, 0)
        if(value < 0) result <- c(result, value, value - 1)
        if(value > 0) result <- c(result, value, value + 1)
      }
      result <- sort(result)[-c(1, length(result))]
      output <- ""
      for(i in seq(1, length(result), 2)) {
        window_start <- .format_window(result[i], input$window_type)
        window_end <- .format_window(result[i + 1], input$window_type)
        output <- paste(output, paste("(", window_start, ", ", window_end, ")", sep = ""), sep = "  ")
      }
      paste0(output, "\n")
    })

    .format_window <- function(days, window_type = "years"){
      months <- round(lubridate::days(days)/months(1), 0)
      months_remaining <- sign(months) * (abs(months) %% 12)

      if(window_type == "months" & months_remaining != 0) {
        years <- floor(lubridate::days(abs(days))/lubridate::years(1))
      } else {
        years <- round(lubridate::days(abs(days))/lubridate::years(1))
      }

      dplyr::case_when(
        years == 0 & months == 0 ~ paste0("0", stringr::str_sub(window_type, 1, 1)),
        years == 0 ~ paste0(months, "m"),
        months == 0 ~ paste0(years, "y"),
        TRUE ~ paste0(sign(days) * years, "y", ifelse(months_remaining != 0, paste0(abs(months_remaining), "m"), ""))
      )
    }

    #
    # reactive for the time windows
    #
    rf_range <- shiny::reactive({
      breaks <- time_windows()
      if(input$zero_window && !(0 %in% breaks)) breaks <- c(0, breaks)
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
      range  <- list(temporalStartDays = startDays, temporalEndDays = endDays)
      return(range)
    })

    return(rf_range)

  })
}
