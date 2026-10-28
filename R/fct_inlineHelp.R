#' Render an inline help control for an analysis settings page
#'
#' @param title Accessible label for the help control.
#' @param ... Help content.
#'
#' @keywords internal
.co2_analysis_help <- function(title = "Show help", ...) {
  htmltools::tags$details(
    class = "co2-inline-help",
    htmltools::tags$summary(
      class = "co2-inline-help-trigger",
      title = title,
      `aria-label` = title,
      shiny::icon("question-circle")
    ),
    htmltools::tags$div(class = "co2-inline-help-content", ...)
  )
}

.co2_analysis_field_help <- function(title = "Help", ...) {
  htmltools::tags$details(
    class = "co2-field-help",
    htmltools::tags$summary(
      class = "co2-field-help-trigger",
      title = title,
      `aria-label` = title,
      shiny::icon("question-circle")
    ),
    htmltools::tags$div(class = "co2-field-help-content", ...)
  )
}

.co2_analysis_field_label <- function(label, title = label, ...) {
  htmltools::tags$div(
    class = "co2-field-label",
    htmltools::tags$span(label),
    .co2_analysis_field_help(title, ...)
  )
}

.co2_analysis_section_heading <- function(label, title = label, ...) {
  htmltools::tags$div(
    class = "co2-section-heading",
    shiny::tags$h4(label),
    .co2_analysis_field_help(title, ...)
  )
}
