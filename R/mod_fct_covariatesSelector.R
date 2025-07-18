
#' Covariate Selector UI Module
#'
#' This module creates a UI component for selecting covariates based on analysis IDs.
#'
#' @param inputId The input ID for the UI component.
#' @param label The label for the UI component.
#' @param analysisIdsToShow A vector of analysis IDs to show in the selector. If NULL, all analysis IDs are shown.
#' @param analysisRegexToShowTibble A tibble with the analysis IDs, names and regex to show in the selector. If NULL, no analysis IDs are shown.
#' @param analysisIdsSelected A vector of analysis IDs to be selected by default. If NULL, all analysis IDs are selected.
#'
#' @return A shiny UI component for selecting covariates.
#' @importFrom dplyr left_join mutate filter select
#' @importFrom tibble tribble
#' @importFrom stringr str_replace_all
#' @importFrom tidyr nest
#' @importFrom purrr map
#' @importFrom shinyWidgets pickerInput
#' @importFrom checkmate assertNumeric assertSubset
#' @export
#'
mod_fct_covariateSelector_ui <- function(inputId, label = NULL, analysisIdsToShow = NULL, analysisRegexToShowTibble = NULL, analysisIdsSelected = NULL) {

  if(is.null(analysisIdsToShow)) {
    analysisIdsToShow  <- HadesExtras::getListOfAnalysis()$analysisId
  }

  if(!is.null(analysisRegexToShowTibble)){
    analysisRegexToShowTibble  |> checkmate::assertDataFrame()
    analysisRegexToShowTibble  |> names() |> checkmate::assertSetEqual(c("analysisId", "analysisName", "analysisRegex"))
  }

   if(is.null(analysisIdsSelected)) {
    analysisIdsSelected <- c(analysisIdsToShow, {if(!is.null(analysisRegexTibble)) analysisRegexTibble |> dplyr::pull(analysisId) else c()})
  }


  checkmate::assertNumeric(analysisIdsToShow)
  checkmate::assertNumeric(analysisIdsSelected)
  checkmate::assertSubset(analysisIdsToShow, HadesExtras::getListOfAnalysis()$analysisId)
  checkmate::assertSubset(analysisIdsSelected, c(analysisIdsToShow, {if(!is.null(analysisRegexToShowTibble)) analysisRegexToShowTibble |> dplyr::pull(analysisId) else c()}))

  analysisNamePretty  <- tibble::tribble(
    ~analysisName, ~analysisNamePretty,
    "ConditionOccurrence", "Conditions",
    "ConditionOccurrencePrimaryInpatient", "Conditions in Primary Inpatient",
    "ConditionEraGroupOverlap", "Conditions SNOMED Group",
    "DrugExposure", "Drugs",
    "DrugEraGroupOverlap", "Drugs Ingredient Group",
    "ATCgroups", "Drugs ATC Group",
    "DDDATCgroups", "Drugs ATC Group total DDDs",
    "ProcedureOccurrence", "Procedures",
    "DeviceExposure", "Device Exposure",
    "Measurement", "Measurement",
    "Observation", "Observation"
  )

  analysisToShow <- HadesExtras::getListOfAnalysis()  |>
    dplyr::left_join(analysisNamePretty, by = c("analysisName" = "analysisName")) |>
    dplyr::mutate(analysisNamePretty = dplyr::if_else(is.na(analysisNamePretty), analysisName, analysisNamePretty)) |>
    dplyr::mutate(analysisNamePretty = stringr::str_replace_all(analysisNamePretty, "([a-z])([A-Z])", "\\1 \\2")) |>
    dplyr::mutate(analysisNamePretty = paste0(analysisNamePretty, " [", dplyr::if_else(isBinary, "Binary", "Continuous"), dplyr::if_else(isSourceConcept, ", Source Codes", ""), "]")) |>
    dplyr::filter(analysisId %in% analysisIdsToShow)|> 
    dplyr::select(group = domainId, analysisId, analysisNamePretty) 

  if(!is.null(analysisRegexToShowTibble)){
    analysisToShow <- dplyr::bind_rows(
      analysisToShow,
      analysisRegexToShowTibble |>
      dplyr::transmute(
        group  = "Cohort Based",
        analysisId = analysisId,
        analysisNamePretty = analysisName
      )
    )
  }

  # named list with group names and sublist of analysisNamePretty and analysisId
  choices <- analysisToShow |>
   tidyr::nest(data = -group) |>
    dplyr::mutate(
      data = purrr::map(data, ~{
        list   <- as.list(.x$analysisId)
        names(list) <- .x$analysisNamePretty
        return(list)
      })
    )

  choicesList  <- as.list(choices$data)
  names(choicesList) <- choices$group

  picker <-  shinyWidgets::pickerInput(
      inputId = inputId,
      label = label,
      choices = choicesList,
      selected = analysisIdsSelected,
      options = list(`actions-box` = TRUE),
      multiple = TRUE)

  return(picker)
}
