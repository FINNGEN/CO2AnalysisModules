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
