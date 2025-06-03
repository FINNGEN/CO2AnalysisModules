#' @title CodeWAS Results Visualization UI
#' @description UI module for visualizing CodeWAS results. This module provides controls to customize the appearance of the plot and options to download the plot and data.
#'
#' @param id A string representing the module's namespace.
#'
#' @return A Shiny UI element that can be included in a Shiny app.
#'
#'
#' @export
#'
mod_resultsVisualisation_PhenotypeScoring_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    title = "Phenotype Scoring Results",
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    shiny::tagList(
      shiny::h4("CodeWAS Results Table"),
      reactable::reactableOutput(ns("codewasResultsTable"), height = 500),
      shiny::actionButton(
        ns("createGroupFromSelected"),
        "Create Group From Selected"
      ),
      shiny::hr(),
      shiny::h4("Code Groups"),
      reactable::reactableOutput(ns("codeGroupsTable"), height = 500),
      shiny::hr(),
      shiny::h4("Groups Overlap"),
      shiny::plotOutput(ns("groupsOverlapPlot"), height = 500)
    )
  ) # end of fluidPage
}


#' @title CodeWAS Results Visualization Server
#' @description Server module for handling the logic of the CodeWAS results visualization UI. This module creates interactive plots and tables based on the analysis results and allows the plot and data to be downloaded.
#'
#' @param id A string representing the module's namespace.
#' @param analysisResults Pooled connection to the analysis results duckdb.
#'
#' @return The module returns server-side logic to generate and manage the CodeWAS results visualization.
#'
#'
#' @export
mod_resultsVisualisation_PhenotypeScoring_server <- function(id, analysisResults) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns


    r <- shiny::reactiveValues(
      allCovariatesTibble = NULL,
      groupOfCovariatesObject = list(
        groupsTibble = tibble::tibble(
          groupId = integer(),
          groupName = character(),
          covariateIds = list(),
          conceptCodes = list(),
          covariateNames = list(),
          covariatesDistribution = list(),
          .rows = 0
        ),
        personGroupsTibble = NULL
      )
    )

    #
    # Start up: get the list of codes from database into r$listOfCovariates
    shiny::observe({
      r$allCovariatesTibble <- .getAllCovariatesTibble(analysisResults)
    })

    #
    # When r$listOfCovariates is ready, plot it
    #
    output$codewasResultsTable <- reactable::renderReactable({
      shiny::req(r$allCovariatesTibble)

      toPlot <- r$allCovariatesTibble |>
        dplyr::transmute(
          domainId = domainId,
          vocabularyId = vocabularyId,
          conceptCode = conceptCode,
          covariateName = stringr::str_remove(covariateName, ".*:"),
          nCasesYes = nCasesYes,
          mplog = -log10(pValue),
          beta = log(oddsRatio),
          isDataAvailable = isDataAvailable
        )

      columns <- list(
        domainId = reactable::colDef(name = "Domain", minWidth = 40),
        vocabularyId = reactable::colDef(name = "Vocabulary", minWidth = 40),
        conceptCode = reactable::colDef(name = "Concept Code", minWidth = 40),
        covariateName = reactable::colDef(name = "Covariate Name", minWidth = 200),
        nCasesYes = reactable::colDef(name = "N Cases", minWidth = 40),
        mplog = reactable::colDef(name = "mplog", minWidth = 40, format = reactable::colFormat(digits = 2)),
        beta = reactable::colDef(name = "beta", minWidth = 40, format = reactable::colFormat(digits = 2)),
        isDataAvailable = reactable::colDef(name = "Data Available", minWidth = 40)
      )

      reactable::reactable(toPlot,
        columns = columns,
        filterable = TRUE,
        sortable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        pagination = TRUE,
        selection = "multiple",
        onClick = "select",
        defaultSorted = list("mplog" = "desc")
      )
    })


    #
    # When click inp$createGroupFromSelected, create a new group into r$groupOfCovariatesObject
    #
    shiny::observeEvent(input$createGroupFromSelected, {
      selected <- reactable::getReactableState("codewasResultsTable", "selected")
      if (!is.null(selected)) {
        # Get the selected rows from the table
        selectedRows <- r$allCovariatesTibble[selected, ]

        # Update the list of groups with selected rows
        r$groupOfCovariatesObject <- .appendCovariateGroup(analysisResults, selectedRows$covariateId, r$groupOfCovariatesObject)
      }

      # clear selection
      reactable::updateReactable("codewasResultsTable", selected = NA)
    })


    #
    # When r$groupOfCovariatesObject is ready, plot it
    #
    output$codeGroupsTable <- reactable::renderReactable({
      shiny::req(r$groupOfCovariatesObject$groupsTibble |> nrow() > 0)

      toPlot <- r$groupOfCovariatesObject$groupsTibble |>
        dplyr::mutate(deleteButton = NA)

      columns <- list(
        groupId = reactable::colDef(show = FALSE),
        groupName = reactable::colDef(name = "Group Name", minWidth = 50),
        covariateIds = reactable::colDef(show = FALSE),
        conceptCodes = reactable::colDef(
          name = "Concept Codes",
          minWidth = 100,
          cell = function(value) {
            paste(value, collapse = "<br>")
          },
          html = TRUE
        ),
        covariateNames = reactable::colDef(
          name = "Covariate Names",
          minWidth = 200,
          cell = function(value) {
            value <- value |>
              stringr::str_remove(".*:") |>
              stringr::str_trunc(80) |>
              paste(collapse = "<br>")
          },
          html = TRUE
        ),
        covariatesDistribution = reactable::colDef(
          name = "Covariates Distribution",
          width = 300,
          cell = .renderCovariatesDistribution
        ),
        deleteButton = reactable::colDef(
          name = "",
          sortable = FALSE,
          cell = function() htmltools::tags$button(shiny::icon("trash")),
          maxWidth = 40
        )
      )

      reactable::reactable(toPlot,
        columns = columns,
        resizable = TRUE
      )
    })

    #
    # When r$groupOfCovariatesObject is ready, plot it
    #
    output$groupsOverlapPlot <- shiny::renderPlot({
      shiny::req(r$groupOfCovariatesObject$groupsTibble |> nrow() > 0)

      columnNames <- r$groupOfCovariatesObject$personGroupsTibble |>
        names() |>
        setdiff("personSourceValue")

      r$groupOfCovariatesObject$personGroupsTibble |>
        dplyr::mutate(dplyr::across(columnNames, ~ ifelse(.x == 0, NA, paste("Group", dplyr::cur_column())))) |>
        dplyr::filter(!dplyr::if_all(columnNames, is.na)) |>
        dplyr::mutate(groups = purrr::pmap(.l = dplyr::across(columnNames), .f = ~ na.omit(c(...)))) |>
        ggplot2::ggplot(aes(x = groups)) +
        ggplot2::geom_bar() +
        ggplot2::geom_text(stat = "count", aes(label = ggplot2::after_stat(count)), vjust = -1) +
        ggupset::scale_x_upset(n_intersections = 20) +
        ggplot2::theme_minimal()
    })
  })
}



.getAllCovariatesTibble <- function(analysisResults) {
  analysisResults |>
    dplyr::tbl("codewasResults") |>
    dplyr::left_join(analysisResults |> dplyr::tbl("covariateRef"), by = c("covariateId" = "covariateId")) |>
    dplyr::left_join(analysisResults |> dplyr::tbl("analysisRef"), by = c("analysisId" = "analysisId")) |>
    # TEMP
    dplyr::filter(!(vocabularyId == "ATC" & nchar(conceptCode) < 7)) |>
    # END TEMP
    dplyr::left_join(
      analysisResults |> dplyr::tbl("covariatesPerPerson") |>
        dplyr::distinct(covariateId) |>
        dplyr::mutate(isDataAvailable = 1),
      by = c("covariateId" = "covariateId")
    ) |>
    dplyr::collect() |>
    dplyr::mutate(isDataAvailable = ifelse(is.na(isDataAvailable), FALSE, TRUE))
}


.appendCovariateGroup <- function(analysisResults, covariateIds, groupOfCovariatesObject) {
  newGroupId <- nrow(groupOfCovariatesObject$groupsTibble) + 1

  sumAllCovariatesPerPerson <- analysisResults |>
    dplyr::tbl("covariatesPerPerson") |>
    dplyr::distinct(personSourceValue) |>
    dplyr::left_join(
      analysisResults |>
        dplyr::tbl("covariatesPerPerson") |>
        dplyr::filter(covariateId %in% covariateIds) |>
        dplyr::group_by(personSourceValue) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop"),
      by = "personSourceValue"
    ) |>
    # TEMP
    dplyr::mutate(value = value + 1) |>
    # END TEMP
    dplyr::mutate(value = ifelse(is.na(value), 0, value)) |>
    dplyr::collect()

  covariatesDistribution <- sumAllCovariatesPerPerson |>
    dplyr::count(value, sort = TRUE)

  # create a new group
  conceptCodes <- analysisResults |>
    dplyr::tbl("covariateRef") |>
    dplyr::filter(covariateId %in% covariateIds) |>
    dplyr::pull(conceptCode)

  covariateNames <- analysisResults |>
    dplyr::tbl("covariateRef") |>
    dplyr::filter(covariateId %in% covariateIds) |>
    dplyr::pull(covariateName)

  groupTibble <- tibble::tibble(
    groupId = newGroupId,
    groupName = paste("Group ", newGroupId),
    covariateIds = list(covariateIds),
    conceptCodes = list(conceptCodes),
    covariateNames = list(covariateNames),
    covariatesDistribution = list(covariatesDistribution)
  )

  sumAllCovariatesPerPerson <- sumAllCovariatesPerPerson |>
    dplyr::rename(!!as.character(newGroupId) := value)

  # append
  groupOfCovariatesObject$groupsTibble <- dplyr::bind_rows(groupOfCovariatesObject$groupsTibble, groupTibble)
  if (is.null(groupOfCovariatesObject$personGroupsTibble)) {
    groupOfCovariatesObject$personGroupsTibble <- sumAllCovariatesPerPerson
  } else {
    groupOfCovariatesObject$personGroupsTibble <- dplyr::left_join(groupOfCovariatesObject$personGroupsTibble, sumAllCovariatesPerPerson, by = "personSourceValue")
  }

  return(groupOfCovariatesObject)
}


.renderCovariatesDistribution <- function(covariatesDistribution) {
  if (is.null(covariatesDistribution)) {
    return(NULL)
  }

  plot <- covariatesDistribution |>
    apexcharter::apex(apexcharter::aes(x = value, y = n), type = "column", height = 150, width = 250) |>
    apexcharter::ax_chart(toolbar = list(show = FALSE)) |>
    apexcharter::ax_legend(show = FALSE)

  return(plot)
}
