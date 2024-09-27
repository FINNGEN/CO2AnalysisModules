#' @title Results Visualization Dashboard UI
#' @description UI module for creating a Shiny dashboard that visualizes results, including cohort overlaps and cohort definitions. This module provides a sidebar with navigation options and a main panel for displaying the content.
#'
#' @param id A string representing the module's namespace.
#' @param resultsVisualisationModuleUi A UI function representing the specific visualization module to include in the main panel.
#' @param pathToAboutMd A string representing the path to a markdown file with information about the application.
#' @param title A string representing the title of the main tab in the sidebar.
#'
#' @return A Shiny UI element that can be included in a Shiny app.
#'
#' @importFrom shiny NS tags icon
#' @importFrom shinydashboard dashboardHeader dashboardSidebar dashboardBody dashboardPage sidebarMenu menuItem tabItems tabItem
#' @importFrom htmltools includeMarkdown tags HTML
#' @importFrom reactable reactableOutput
#'
#' @export
#'
mod_resultsVisualisation_ui <- function(id, resultsVisualisationModuleUi, pathToAboutMd, title, logshref) {
  ns <- shiny::NS(id)

  headerContent <- shiny::tags$li(
    class = "dropdown",
    style = "margin-top: 8px !important; margin-right : 5px !important"
  )

  header <-
    shinydashboard::dashboardHeader(title = title, headerContent)

  sidebarMenu <-
    shinydashboard::sidebarMenu(
      id = ns("tabs"),
      shinydashboard::menuItem(text = title, tabName = "module", icon = shiny::icon("table")),
      shinydashboard::menuItem(text = "About", tabName = "about"),
      shinydashboard::menuItem(text = "Study details", tabName = "cohortsInfo"),
      shinydashboard::menuItem("App Logs", icon = shiny::icon("info-circle"), href = logshref),
      selected = "module"
    )

  # Side bar code
  sidebar <-
    shinydashboard::dashboardSidebar(sidebarMenu,
                                     width = NULL,
                                     collapsed = FALSE
    )

  bodyTabItems <- shinydashboard::tabItems(
    shinydashboard::tabItem(
      tabName = "about",
      htmltools::includeMarkdown(pathToAboutMd)
    ),
    shinydashboard::tabItem(
      tabName = "cohortsInfo",
      shiny::tags$h4("Cohort Information"),
      reactable::reactableOutput(ns("cohortsInfo")),
      shiny::tags$h4("Analysis Information"),
      reactable::reactableOutput(ns("analysisInfo")),
      shiny::tags$h4("Database Information"),
      reactable::reactableOutput(ns("databaseInfo"))
    ),
    shinydashboard::tabItem(
      tabName = "module",
      shiny::tags$h4("Cohorts"),
      shiny::div(
        style = "margin-left: 3px; margin-top: 10px; margin-right:3px; margin-bottom: 20px;",
        reactable::reactableOutput(ns("usedCohortsInfo"))
      ),
      resultsVisualisationModuleUi(ns(id)),
    )
  )

  # body
  body <- shinydashboard::dashboardBody(
    bodyTabItems
  )

  # main
  ui <- shinydashboard::dashboardPage(
    title = title,
    header = header,
    sidebar = sidebar,
    body = body
  )

  return(ui)

}


#' @title Results Visualization Dashboard Server
#' @description Server module for handling the logic of the results visualization dashboard.
#' This includes rendering the cohort definitions table and invoking the specific visualization module's server logic.
#'
#' @param id A string representing the module's namespace.
#' @param resultsVisualisationModuleServer A server function representing the specific visualization module's server logic.
#' @param analysisResults Pooled connection to the analysis results duckdb.
#'
#' @return The module returns server-side logic to manage the results visualization dashboard.
#'
#' @importFrom shiny moduleServer
#' @importFrom dplyr tbl select collect
#' @importFrom reactable renderReactable
#'
#' @export
#'
mod_resultsVisualisation_server <- function(id, resultsVisualisationModuleServer, analysisResults) {


  shiny::moduleServer(id, function(input, output, session) {

    output$analysisInfo <- reactable::renderReactable({
      analysisInfoTable <- analysisResults |> dplyr::tbl('analysisInfo') |> dplyr::collect()
      reactable::reactable(analysisInfoTable)
    })

    output$databaseInfo <- reactable::renderReactable({
      databaseInfoTable <- analysisResults |> dplyr::tbl('databaseInfo') |> dplyr::collect()
      reactable::reactable(databaseInfoTable)
    })

    output$cohortsInfo <- reactable::renderReactable({
        cohortsInfo <- analysisResults |> dplyr::tbl('cohortsInfo') |>
          dplyr::select(-sql, -json) |>
          dplyr::collect()
        reactable::reactable(cohortsInfo)
    })

    output$usedCohortsInfo <- reactable::renderReactable({
      countsTable <- analysisResults |> dplyr::tbl('cohortsInfo') |>
        dplyr::filter(!is.na(use) & use != "") |>
        dplyr::select(shortName, cohortName, cohortSubjects, cohortEntries) |>
        dplyr::collect()

      reactable::reactable(countsTable)
    })

    resultsVisualisationModuleServer(id, analysisResults)
  })


}
