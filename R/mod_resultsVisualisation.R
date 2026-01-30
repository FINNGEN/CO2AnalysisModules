#' @title Results Visualization Dashboard UI
#' @description UI module for creating a Shiny dashboard that visualizes results, including cohort overlaps and cohort definitions. This module provides a sidebar with navigation options and a main panel for displaying the content.
#'
#' @param id A string representing the module's namespace.
#' @param resultsVisualisationModuleUi A UI function representing the specific visualization module to include in the main panel.
#' @param pathToAboutMd A string representing the path to a markdown file with information about the application.
#' @param title A string representing the title of the main tab in the sidebar.
#' @param logshref A string representing the URL to the logs page.
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
      reactable::reactableOutput(ns("databaseInfo")),
      shiny::tags$hr(),
      shiny::tags$h5("Download the entire analysis result (.duckdb)"),
      shiny::downloadButton(ns("download_results_duckdb"), "Download results")
    ),
    shinydashboard::tabItem(
      tabName = "module",
      shiny::uiOutput(ns("usedCohortsInfo")),
      resultsVisualisationModuleUi(ns(id))
    ) # end of tabItem
  ) # end of tabItems


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

    output$usedCohortsInfo <- shiny::renderUI({
      countsTable <- analysisResults |> dplyr::tbl('cohortsInfo') |>
        dplyr::filter(!is.na(use) & use != "") |>
        dplyr::select(use, shortName, cohortName, cohortSubjects, cohortEntries) |>
        dplyr::collect()

      shiny::tagList(
        tags$head(
          tags$style(HTML("
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
        .container-fluid {
          padding: 0px;
        }
      "))),
        tags$script(HTML("
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
         ") # end of HTML
        ), # end of tags$script
        div(class = "menu-section",
            div(class = "collapsible-header",
                onclick = "toggleSection(this)",
                tags$span(class = "triangle", "\u25B6"),  # ▶
                style = "font-size: 16px; font-weight: normal; padding: 10px; ",
                "Cohorts"
            ),
            div(class = "collapsible-content",
                reactable::reactable(countsTable)
              )
          )# end of div
      )# end of tagList
    })

    output$download_results_duckdb <- shiny::downloadHandler(
      filename = function() {
        analysisType <- getOption("CO2AnalysisModules.analysisType", default = "analysis")
        sanitized <- gsub("[^[:alnum:]]+", "_", analysisType)
        sanitized <- gsub("^_+|_+$", "", sanitized)
        paste0(sanitized, "_analysisResults.duckdb")
      },
      content = function(fname) {
        db_path <- getOption("CO2AnalysisModules.pathToResultsDatabase", default = "")
        shiny::req(nzchar(db_path))
        shiny::req(file.exists(db_path))

        ok <- file.copy(db_path, fname, overwrite = TRUE)
        if (!isTRUE(ok) || file.info(fname)$size == 0) {
          stop("Download failed: could not copy DuckDB file.")
        }

        ParallelLogger::logInfo("[viewer] Download duckdb: ", db_path)
      }
    )



    resultsVisualisationModuleServer(id, analysisResults)
  })


}
