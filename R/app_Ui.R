#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tagList actionButton
#' @importFrom shinyjs useShinyjs hidden
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    shinyjs::useShinyjs(),
    # hidden button triggered when the new module-ui is flushed to call the module-server
    # also serves as reference to insert the new module-ui under it
    shinyjs::hidden(
      shiny::actionButton("hidenButton", "hidenButton")
    )
  )

}
