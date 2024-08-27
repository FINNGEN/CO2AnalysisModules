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
#' @importFrom htmltools includeMarkdown tags style HTML
#' @importFrom reactable reactableOutput
#'
#' @export
#'
mod_resultsVisualisation_ui <- function(id, resultsVisualisationModuleUi, pathToAboutMd, title) {
  ns <- shiny::NS(id)

  headerContent <- shiny::tags$li(
    class = "dropdown",
    style = "margin-top: 8px !important; margin-right : 5px !important"
  )

  header <-
    shinydashboard::dashboardHeader(title = "cohortOverlaps", headerContent)

  sidebarMenu <-
    shinydashboard::sidebarMenu(
      id = ns("tabs"),
      shinydashboard::menuItem(text = title, tabName = "module", icon = shiny::icon("table")),
      shinydashboard::menuItem(text = "About", tabName = "about", icon = shiny::icon("code")),
      # shinydashboard::menuItem(text = "Cohort Definition", tabName = "cohortDefinition", icon = shiny::icon("code")),
      shinydashboard::menuItem(text = "Analysis Information", tabName = "analysisinfo", icon = shiny::icon("code")),
      shinydashboard::menuItem(text = "Database Information", tabName = "database", icon = shiny::icon("code")),
      selected = "module"
      #shinydashboard::menuItem(text = "Meta data", tabName = "databaseInformation", icon = shiny::icon("gear", verify_fa = FALSE))
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
    # shinydashboard::tabItem(
    #   tabName = "cohortDefinition",
    #   reactable::reactableOutput(ns("cohortDefinitions"))
    # ),
    shinydashboard::tabItem(
      tabName = "analysisinfo",
      reactable::reactableOutput(ns("analysisInfo"))
    ),
    shinydashboard::tabItem(
      tabName = "database",
      reactable::reactableOutput(ns("databaseInfo"))
    ),
    shinydashboard::tabItem(
      tabName = "module",
      resultsVisualisationModuleUi(ns(id)),
      shiny::div(
        style = "margin-left: 20px; margin-top: 50px; margin-right:20px;",
        reactable::reactableOutput(ns("cohortDefinitions"))
      ),
    )
  )

  # body
  body <- shinydashboard::dashboardBody(
    bodyTabItems
  )

  # main
  ui <- shinydashboard::dashboardPage(
    shiny::tags$head(shiny::tags$style(htmltools::HTML(
      "
        th, td {
          padding-right: 10px;
        }

      "
    ))),
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
#' @param analysisResults Pooled connection to the analisys results duckdb.
#'
#' @return The module returns server-side logic to manage the results visualization dashboard.
#'
#' @importFrom shiny moduleServer renderReactable
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
      databaseInfoTable <- analysisResults |> dplyr::tbl('database') |> dplyr::collect()
      reactable::reactable(databaseInfoTable)
    })

    output$cohortDefinitions <- reactable::renderReactable({
      countsTable <- analysisResults |> dplyr::tbl('cohortCounts') |> dplyr::collect()
      definitionTable <- analysisResults |> dplyr::tbl('CohortDefinitionSet') |> dplyr::select(-sql, -json) |> dplyr::collect()

      reactable::reactable(
        dplyr::full_join(countsTable, definitionTable, by = c('cohortId', 'cohortName')) |>
          dplyr::select(cohortId, cohortName, shortName, cohortSubjects, subsetParent, isSubset, subsetDefinitionId)
      )
    })

    resultsVisualisationModuleServer(id, analysisResults)
  })


}


































