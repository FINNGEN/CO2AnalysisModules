#' @title Cohort TimeCodeWAS Visualization UI
#' @description UI module for visualizing cohort overlaps using an UpSet plot. This module provides controls to customize the appearance of the plot and options to download the plot and data.
#'
#' @param id A string representing the module's namespace.
#'
#' @return A Shiny UI element that can be included in a Shiny app.
#'
#' @importFrom shiny NS tagList tags h4 uiOutput tabsetPanel tabPanel div downloadButton
#' @importFrom ggiraph girafeOutput
#' @importFrom DT DTOutput
#'
#' @export
#'
mod_resultsVisualisation_TimeCodeWAS_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    title = "TimeCodeWAS",
    shiny::tagList(
      shiny::tags$h4("Filters"),
      shiny::uiOutput(ns("outputUI")),
      shiny::tags$h4("Data"),
      shiny::tabsetPanel(
        id = ns("tabset"),
        shiny::tabPanel(
          "Plot",
          shiny::div(style = "margin-top: 10px; ",
                     shiny::checkboxInput(ns("top_10"), "Label top 10", value = TRUE),
          ),
          ggiraph::girafeOutput(ns("codeWASplot"), width = "100%", height = "100%"),
          shiny::div(
            style = "margin-top: 10px; margin-bottom: 10px;",
            shiny::downloadButton(ns("downloadPlot"), "Download")
          ),
        ),
        shiny::tabPanel(
          "Table",
          shiny::div(
            style = "margin-top: 20px; margin-bottom: 10px;",
            DT::DTOutput(ns("demographicsData")),
          ),
          shiny::div(
            style = "margin-top: 10px; margin-bottom: 10px;",
            shiny::downloadButton(ns("downloadDataFiltered"), "Download filtered"),
            shiny::downloadButton(ns("downloadDataAll"), "Download all"),
          )
        )
      )
    )
  )

}

#' @title Cohort Demographics Visualization Server
#' @description Server module for handling the logic of the cohort overlaps visualization UI. This module creates an UpSet plot based on the analysis results and allows the plot and data to be downloaded.
#'
#' @param id A string representing the module's namespace.
#' @param analysisResults Pooled connection to the analysis results duckdb.
#'
#' @return The module returns server-side logic to generate and manage the cohort overlaps UpSet plot.
#'
#' @importFrom shiny moduleServer reactive req renderUI observeEvent downloadHandler
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom shinyWidgets pickerInput pickerOptions chooseSliderSkin
#' @importFrom ggiraph renderGirafe girafe opts_sizing opts_hover opts_selection opts_toolbar geom_point_interactive
#' @importFrom ggrepel geom_text_repel
#' @importFrom DT renderDataTable datatable formatSignif formatStyle
#' @importFrom dplyr filter mutate select arrange transmute left_join pull case_when if_else inner_join row_number
#' @importFrom tidyr separate
#' @importFrom stringr str_remove_all str_remove str_c str_wrap str_trunc str_split str_extract_all str_sub
#' @importFrom scales percent number rescale
#' @importFrom lubridate now days
#' @importFrom readr write_csv
#' @importFrom purrr map2_chr
#' @importFrom grid unit gpar
#' @importFrom gtable gtable_add_grob
#' @importFrom ggplot2 ggplot aes geom_segment scale_size_manual scale_x_continuous scale_y_continuous facet_grid theme_minimal theme scale_color_manual scale_fill_discrete guides labs ggplot_build ggplot_gtable
#' @importFrom ggplotify as.ggplot
#' @importFrom grDevices cairo_pdf dev.off
#' @importFrom shinyjs toggle
#'
#' @export
#'
mod_resultsVisualisation_TimeCodeWAS_server <- function(id, analysisResults) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    atlasUrl <- shiny::getShinyOption("cohortOperationsConfig")$atlasUrl # or "TEMP"

    studyResults  <- .analysisResultsHandler_to_studyResults(analysisResults)

    last_plot <- NULL

    # fixed values
    time_periods = .get_time_periods(studyResults)
    gg_data_saved = .studyResults_to_gg_data(studyResults)

    # reactive values
    r <- shiny::reactiveValues(
      gg_data = NULL,
      gg_plot = NULL,
      #
      line_to_plot = NULL,
      force_update = FALSE,
    )

    #
    # render the UI
    #
    output$outputUI <- shiny::renderUI({
      shiny::tagList(
        shinyFeedback::useShinyFeedback(),
        shiny::fluidRow(
          shiny::column(
            2,
            shinyWidgets::pickerInput(
              ns("selected_domains"),
              "Observation type",
              choices = unique(gg_data_saved$domain),
              selected = unique(gg_data_saved$domain),
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                selectedTextFormat = 'count',
                countSelectedText = '{0} observation types'
              ),
              multiple = TRUE
            )
          ),
          # shiny::column(
          #   2,
          #   shinyWidgets::pickerInput(
          #     ns("selected_p_groups"),
          #     "p-value groups",
          #     choices = c(
          #       "-log10(p) [0,50]" = 1,
          #       "-log10(p) (50,100]" = 5,
          #       "-log10(p) (100,200]" = 10,
          #       "-log10(p) (200,Inf]" = 20
          #       ),
          #     selected = c(1, 5, 10, 20),
          #     options = shinyWidgets::pickerOptions(
          #       actionsBox = TRUE,
          #       selectedTextFormat = 'count',
          #       countSelectedText = '{0} p-value groups'
          #     ),
          #     multiple = TRUE
          #   ),
          # ), # column
          shiny::column(
            2,
            shiny::textInput(
              inputId = ns("p_value_threshold"),
              label =  "p-value threshold",
              value = "1e-5",
              width = "100%"
            ),
          ), # column
          shinyWidgets::chooseSliderSkin("Flat"),
          shiny::column(
            width = 2, align = "left",
            shiny::div(style = "height: 85px; width: 100%; margin-top: -15px;",
                       shiny::sliderInput(ns("or_range"), "OR range filtered out", min = 0.0, max = 2, value = c(0.8,1.2), step = 0.1),
            )
          ),
          shiny::column(
            width = 2, align = "left",
            shiny::div(style = "height: 85px; width: 100%; margin-top: -15px; margin-right: 20px;",
                       shiny::sliderInput(ns("n_cases"), "Minimum # of cases", min = 0, max = 1000, value = 0, step = 1),
            ),
          ), # column
          shiny::column(
            width = 2, align = "left",
            shiny::div(style = "width: 100%; margin-top: 30px; margin-right: 20px;",
                       shiny::checkboxInput(ns("na_anywhere"), "Allow NA", value = FALSE),
            ),
          ) # column
        )
      ) # fluidRow
    })

    is_valid_number <- function(input_string) {
      # This regex matches regular and scientific notation numbers
      return(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", input_string))
    }

    #
    # observe the selected domains and p_groups
    #
    shiny::observe({
      shiny::req(input$selected_domains)
      shiny::req(input$or_range)
      shiny::req(input$n_cases)
      shiny::isTruthy(input$na_anywhere)

      if(!is_valid_number(input$p_value_threshold)) {
        shinyFeedback::showFeedbackWarning(
          inputId = "p_value_threshold",
          text = "Invalid input: Please give a valid number."
        )
      } else if(input$p_value_threshold == "") {
        shinyFeedback::showFeedbackWarning(
          inputId = "p_value_threshold",
          text = "Missing input: Please give a number (1e-5 is the default).")
      } else {
        shinyFeedback::hideFeedback("p_value_threshold")
        #
        # filter data
        #
        gg_data <- gg_data_saved |>
          dplyr::filter(domain %in% input$selected_domains) |>
          dplyr::filter(p < as.numeric(input$p_value_threshold) | is.na(p)) |>
          dplyr::filter(OR <= input$or_range[1] | OR >= input$or_range[2] | is.na(OR)) |>
          dplyr::filter(nCasesYes >= input$n_cases) |>
          dplyr::filter(!dplyr::if_any(dplyr::everything(), is.na) | input$na_anywhere)

        # update gg_data
        r$gg_data <- gg_data
      }

    })

    #
    # updates ggirafe plot when r$gg_data or r$show_labels or r$show_labels_cases_per changes
    #
    # renderGirafe ####
    #
    output$codeWASplot <- ggiraph::renderGirafe({
      shiny::req(r$gg_data)
      r$force_update

      gg_girafe <- .gg_data_to_gg_girafe(
        gg_data = r$gg_data,
        selection = r$line_to_plot,
        r = r,
        top_10 = input$top_10
      )

      return(gg_girafe)
    })

    #
    # triggered when points selected by click or by lasso
    # when plot is redrawn this is also triggered
    # this block captures the new selected points and then updates r$selected
    #
    # codeWASplot_selected ####
    #
    shiny::observeEvent(input$codeWASplot_selected, {
      shiny::req(input$codeWASplot_selected)
      shiny::req(input$codeWASplot_selected != "NA")

      # clean selection value take only last selected
      selected_rows <- input$codeWASplot_selected
      selected_rows <- selected_rows[selected_rows != ""]
      selected_rows <- selected_rows[selected_rows != "NA"]

      # ignore selected rows that are currently plot as a line
      selected_rows  <- setdiff(selected_rows, r$line_to_plot$data_id)

      # was the line unselected?
      if(length(selected_rows) == 0){
        r$line_to_plot <- NULL
        return()
      }

      line_to_plot <- NULL

      if(length(selected_rows) > 1){
        # we have a marquee selection with n > 1
        df_lasso <- r$gg_data |>
          dplyr::filter(data_id %in% selected_rows)

        # show table
        shiny::showModal(
          shiny::modalDialog(
            shiny::div(
            tags$style(HTML(".modal-dialog {width: 90%; max-width: 90%;}")),
            DT::renderDataTable({
              .renderTable(df_lasso)
            }),
            size = "l",
            easyClose = FALSE,
            title = paste0("Entries (", nrow(df_lasso), ")"),
            footer = shiny::modalButton("Close"),
            options = list(
              autowidth = TRUE
            )
          ))
        )
        r$line_to_plot <- NULL
        r$force_update <- !r$force_update
      } else {
        # single point selected, either by click or marquee
        selected_rows_clean <- stringr::str_remove_all(selected_rows, "@.*")
        line_to_plot <- r$gg_data |>
          dplyr::filter(code %in% selected_rows_clean) |>
          dplyr::arrange(code, time_period) |>
          dplyr::mutate(position = match(time_period, time_periods)) |>
          dplyr::mutate(name = ifelse(!is.na(position), paste0("panel-1-", position), "NA")) |>
          dplyr::select(code, domain, name, cases_per, controls_per, data_id)
        # update reactive values
        r$line_to_plot <- line_to_plot
      }

    }, ignoreInit = TRUE)

    #
    # render the table ####
    #
    .renderTable <- function(df){
      df <- df |>
        dplyr::mutate(meanCases = round(meanCases, 3)) |>
        dplyr::mutate(meanControls = round(meanControls, 3))|>
        dplyr::mutate(sdCases = round(sdCases, 3)) |>
        dplyr::mutate(sdControls = round(sdControls, 3))|>
        dplyr::mutate(beta = round(log(OR), 3)) |>
        dplyr::mutate(
          code = round(code/1000),
          name = purrr::map2_chr(name, code, ~paste0('<a href="',atlasUrl,'/#/concept/', .y, '" target="_blank">', .x,'</a>'))
        ) |>
        dplyr::select(
          GROUP, name, analysisName, domain, upIn,
          nCasesYes, nControlsYes, meanCases, meanControls, sdCases, sdControls,
          OR, p, beta, notes)

        DT::datatable(
          df,
          colnames = c(
            'Time ID' = 'GROUP',
            'Covariate Name' = 'name',
            'Analysis Name' = 'analysisName',
            'Domain' = 'domain',
            'Type' = 'upIn',
            'N case' = 'nCasesYes',
            'N ctrl' = 'nControlsYes',
            'Mean case' = 'meanCases',
            'Mean ctrl' = 'meanControls',
            'SD case' = 'sdCases',
            'SD ctrl' = 'sdControls',
            'OR' = 'OR',
            'p' = 'p',
            'Beta' = 'beta',
            'Notes' = 'notes'
          ),
          options = list(
            order = list(list(12, 'asc'), list(11, 'desc')) # order by p-value, then OR
            # columnDefs = list(
            #   list(width = '70px', targets = c(2, 3, 15)), # name, notes
            #   list(width = '80px', targets = c(1)), # analysisName
            #   list(width = '80px', targets = c(4)), # domain
            #   list(width = '30px', targets = c(4)), # upIn
            #   list(width = '40px', targets = c(6,7,8, 9, 10,11)), # GROUP, nCasesYes, nControlsYes, meanCases, meanControls, sdCases, sdControls
            #   list(width = '50px', targets = c(12, 13)) # pValue, OR
            # )
          ),
          escape = FALSE,
          selection = 'none',
          rownames = FALSE
        ) |>
        DT::formatSignif(columns = c('p', 'OR'), digits = 3) |>
        DT::formatStyle('Covariate Name', cursor = 'pointer' )
    }

    #
    # output "Table" tab ####
    #
    output$demographicsData <- DT::renderDataTable({
      shiny::req(r$gg_data)

      .renderTable(r$gg_data)
    })

    #
    # download filtered data as a table
    #
    output$downloadDataFiltered <- shiny::downloadHandler(
      filename = function(){
        paste('timecodewas_filtered_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.csv', sep='')
      },
      content = function(fname){
        readr::write_csv(r$gg_data |> dplyr::select(-label), fname)
        return(fname)
      }
    )

    #
    # download all data as a table
    #
    output$downloadDataAll <- shiny::downloadHandler(
      filename = function(){
        paste('timecodewas_all_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.csv', sep='')
      },
      content = function(fname){
        readr::write_csv(gg_data_saved |> dplyr::select(-label), fname)
        return(fname)
      }
    )

    #
    # download data as a plot
    #
    output$downloadPlot <- shiny::downloadHandler(
      filename = function(){
        paste('timecodewas_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.pdf', sep='')
      },
      content = function(fname){

        grDevices::cairo_pdf(filename = fname,
                             width = 15,
                             height = 5,
                             pointsize = 1.0,
                             family = "sans",
                             bg = "transparent",
                             antialias = "default",
                             fallback_resolution = 300,
        )
        print(r$gg_plot)
        grDevices::dev.off()
      },
      contentType = "application/pdf"
    )
  })
} # mod_timeCodeWASPlot_server

# utility functions

.label_editor <- function(s){
  for(i in 1:length(s)){
    s[[i]] <- .label_editor_single(s[[i]])
  }
  return(s)
}

#
# label facet periods as months
#

.label_editor_single <- function(s){
  limits <- as.numeric(stringr::str_extract_all(s, "[-]*\\d+")[[1]])
  from <- round(lubridate::days(limits[1])/months(1), 1)
  to <- round(lubridate::days(limits[2])/months(1), 1)
  s <- paste0(from, " / ", to, "\nmonths")
  return(s)
}

.get_time_periods <- function(studyResult){
  l <- unique(studyResult |> dplyr::filter(!is.na(startDay) & !is.na(endDay)) |> dplyr::pull(timeRange))
  l_split <- lapply(l, function(x) {stringr::str_split(x, " ", simplify = TRUE)})
  time_periods <- as.data.frame(do.call(rbind, l_split)) |>
    dplyr::arrange(as.numeric(V2)) |>
    dplyr::mutate(period = paste(V1,V2,V3,V4)) |>
    dplyr::pull(period)

  return(time_periods)
}


.studyResults_to_gg_data <- function(studyResult){

  time_periods <- .get_time_periods(studyResult)

  studyResult <- studyResult |>
    dplyr::transmute(
      code = covariateId,
      time_period = factor(timeRange, levels = time_periods, labels = time_periods),
      name = covariateName,
      analysisName = analysisName,
      model = modelType,
      notes = runNotes,
      OR=OR,
      p=p,
      upIn=upIn,
      nCasesInWindow = nCasesInWindow,
      nControlsInWindow = nControlsInWindow,
      cases_per = nCasesYes/nCasesInWindow,
      meanCases = nCasesYes/nCasesInWindow,
      meanControls = nControlsYes/nControlsInWindow,
      sdCases = sdCases,
      sdControls = sdControls,
      controls_per = nControlsYes/nControlsInWindow,
      nCasesYes = nCasesYes,
      nControlsYes = nControlsYes
    ) |>
    tidyr::separate(name, c("domain", "name"), sep = ":", extra = "merge") |>
    dplyr::mutate(name = stringr::str_remove(name, "^[:blank:]")) |>
    dplyr::mutate(p = dplyr::if_else(p==0, 10^-323, p))

  gg_data <- studyResult |>
    dplyr::arrange(time_period, name) |>
    dplyr::mutate_if(is.character, stringr::str_replace_na, "") |>
    dplyr::mutate(
      GROUP = time_period,
      label = stringr::str_c(code),
      label = stringr::str_remove(label, "[:blank:]+$"),
      label = stringr::str_c(domain, " : ", name,
                             "\n-log10(p)=", scales::number(-log10(p), accuracy = 0.1) ,
                             "\n log10(OR) = ", ifelse(is.na(OR), "", scales::number(log10(OR), accuracy = 0.1)),
                             "\n cases:", nCasesYes, " (", scales::percent(cases_per, accuracy = 0.01), ")",
                             "\n controls:", nControlsYes, " (", scales::percent(controls_per, accuracy = 0.01), ")"
      ),
      link = paste0("https://atlas.app.finngen.fi/#/concept/", stringr::str_sub(code, 1, -4)),
      upIn = upIn,
      id = dplyr::row_number(),
      p_group = cut(-log10(p),
                    breaks = c(-1, 50, 100, 200, Inf ),
                    labels = c("-log10(p) [0,50]", "-log10(p) (50,100]", "-log10(p) (100,200]", "-log10(p) (200,Inf]"),
                    ordered_result = TRUE
      ),
      p_group_size = dplyr::case_when(
        as.integer(p_group)==1 ~ 1L,
        as.integer(p_group)==2 ~ 5L,
        as.integer(p_group)==3 ~ 10L,
        as.integer(p_group)==4 ~ 20L,
      ),
      log10_OR = dplyr::case_when(
        log10(OR) == -Inf ~ -2.5,
        log10(OR) == Inf ~ 5,
        TRUE ~ log10(OR) ,
      ),
      data_id = paste0(code, "@", as.character(time_period)),
      data_id_class = code
    )

  return(gg_data)
}



.gg_data_to_gg_girafe <- function(
    gg_data,
    # show_labels,
    # show_labels_cases_per,
    selection,
    r,
    top_10
){
  # adjust the label area according to facet width
  facet_max_x <- max( gg_data$controls_per, 0.03, na.rm = TRUE)
  facet_max_y <- max( gg_data$cases_per, 0.03, na.rm = TRUE)
  #
  #
  gg_fig <- ggplot2::ggplot(
    data = dplyr::arrange( gg_data, log10_OR),
    ggplot2::aes(
      y = cases_per, #log10_OR, # cases_per-controls_per,#log10_OR,# -log10(p), cases_per-controls_per,#
      x = controls_per, #-log10(p), # 1, # id, #log10_OR,
      color = "darkgray",
      fill = domain,
      tooltip = label,
      # size = ordered(p_group), # log10_OR
      data_id = data_id
      # onclick = paste0('window.open("', link , '")')
    ), alpha = 0.75)+
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, y = 0,
                   xend = ifelse(facet_max_x > facet_max_y, facet_max_y, facet_max_x),
                   yend = ifelse(facet_max_x > facet_max_y, facet_max_y, facet_max_x)
      ),
      color = "darkgray", alpha = 0.5, linewidth = 0.2, linetype = "dashed") +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, y = 0, xend = facet_max_x, yend = 0),
      color = "black", alpha = 0.5, linewidth = 0.2, linetype = "dashed") +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, y = 0, xend = 0, yend = facet_max_y),
      color = "black", alpha = 0.5, linewidth = 0.2, linetype = "dashed") +
    ggiraph::geom_point_interactive(
      ggplot2::aes(size = p_group), show.legend=T, shape = 21) + #, position = position_dodge(width = 12))+
    ggplot2::scale_size_manual(
      values = c(
        "-log10(p) [0,50]" = 1,
        "-log10(p) (50,100]" = 1.5,
        "-log10(p) (100,200]" = 2,
        "-log10(p) (200,Inf]" = 3
      )
    ) +
    {if(length(selection) > 1)
      # label the selected points
      ggrepel::geom_text_repel(
        data =  gg_data |>
          dplyr::filter(data_id %in% selection$data_id),
        ggplot2::aes(label = stringr::str_wrap(stringr::str_trunc(name, 30), 15)),
        max.overlaps = Inf,
        size = 3,
        hjust = 0.1,
        force = 0.5,
        force_pull = 0.5,
        xlim = c(facet_max_x / 4, NA),
        box.padding = 0.8
      )} +
    {if((length(selection) == 0) & top_10)
      # label the top 10 values
      ggrepel::geom_text_repel(
        data =  gg_data |>
          dplyr::arrange(p, OR) |>
          dplyr::slice_head(n = 10),
        ggplot2::aes(label = stringr::str_wrap(stringr::str_trunc(name, 45), 30)),
        max.overlaps = Inf,
        size = 3,
        hjust = 0.1,
        force = 0.5,
        force_pull = 0.5,
        xlim = c(facet_max_x / 4, NA),
        box.padding = 0.8
      )} +
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.05, seq(0.1, 0.8, 0.1)),
      labels = c(0, 5, seq(10, 80, 10)),
      limits = c(-0.02 * facet_max_x, facet_max_x)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 0.05, seq(0.1, 0.8, 0.1)),
      labels = c(0, 5, seq(10, 80, 10)),
      limits = c(-0.02 * facet_max_y, facet_max_y)
    ) +
    # ggplot2::coord_fixed() +
    ggplot2::facet_grid(
      .~GROUP, drop = FALSE, scales = "fixed",
      labeller = ggplot2::labeller(GROUP = .label_editor)
    )+
    ggplot2::theme_minimal()+
    ggplot2::theme(
      legend.key.height = grid::unit(3, "mm"),
      legend.key.width = grid::unit(7, "mm"),
      legend.title = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 8),
      legend.position = "bottom",
      legend.direction = "vertical",
      strip.text.x = ggplot2::element_text(size = 10)
    ) +
    ggplot2::scale_color_manual(values = c("darkgray")) +
    ggplot2::scale_fill_discrete() +
    # ggplot2::scale_fill_manual(values = c(
    #   "drug_era_group" = "green",
    #   "drug_exposure" = "lightblue2",
    #   "condition_occurrence" = "khaki",
    #   "condition_era_group" = "blue",
    #   "observation" = "orange",
    #   "measurement" = "palegreen",
    #   "procedure_occurrence" = "plum1"
    # ),
    #   labels = c(
    #     "drug_era_group" = "Drug era",
    #     "drug_exposure" = "Drug exposure",
    #     "condition_occurrence" = "Condition occurrence",
    #     "condition_era_group" = "Condition era",
    #     "observation" = "Observation",
    #     "measurement" ="Measurement",
    #     "procedure_occurrence" = "Procedure occurrence"
    #   )
    # ) +
    ggplot2::guides(color = "none", fill = ggplot2::guide_legend(override.aes = list(size = 5), nrow=2, byrow=TRUE)) +
    ggplot2::labs(size = "p value group", fill = "Domain", x = "\nControls %", y = "Cases %")

  r$gg_plot <- gg_fig

  selected_items <- ""

  if(!is.null(selection) && length(unique(selection$code)) == 1){
    grDevices::pdf(NULL)
    # one point selected -> draw a line connecting the same code in each facet
    gb <- ggplot2::ggplot_build(gg_fig)
    g <- ggplot2::ggplot_gtable(gb)
    # remove domains not in the current data
    selection <-  selection |>
      dplyr::filter(domain %in%  gg_data$domain)
    # check if we have lines to draw
    if(nrow(selection) > 1){
      z_val = 0
      ranges <- gb$layout$panel_params
      data2npc <- function(x, range) scales::rescale(c(range, x), c(0,1))[-c(1,2)]
      x_range <-  ranges[[1]][["x.range"]]
      y_range <- ranges[[1]][["y.range"]]

      selection <- dplyr::inner_join(selection, g$layout, by = "name") |>
        dplyr::mutate(controls_per = data2npc(controls_per, x_range)) |>
        dplyr::mutate(cases_per = data2npc(cases_per, y_range))
      selection$z <- 1
      selection$clip <- "off"

      # move to the beginning of selection
      g <- gtable::gtable_add_grob(
        g, grid::moveToGrob(selection[1,]$controls_per, selection[1,]$cases_per),
        t = selection[1,]$t, selection[1,]$l, z = z_val)
      # draw the lines
      for(i in 2:nrow(selection)){
        if(is.na(selection[i,]$t) || is.na(selection[i,]$l))
          next
        g <- gtable::gtable_add_grob(
          g, grid::lineToGrob(selection[i,]$controls_per, selection[i,]$cases_per, gp = grid::gpar(col = "red", alpha = 0.3, lwd = 2.5)),
          t = selection[i,]$t, selection[i,]$l, z = z_val)
      }

      # turn clip off to see the line across panels
      g$layout$clip <- "off"
    }

    if(!is.null(selection) & length(selection) == 1){
      selected_items <- as.character(unique(selection$code))
      # extend selection to same code in all facets
      selected_items <- gg_data |>
        dplyr::filter(code == selected_items) |>
        dplyr::pull(data_id)
      skip_selection <- TRUE
    } else {
      selected_items <- ""
      skip_selection <- FALSE
    }

    gg_plot <- ggplotify::as.ggplot(g)
  } else {
    skip_selection <- FALSE
    gg_plot <- gg_fig
  }

  #
  # convert ggplot to girafe object
  #
  gg_girafe <- ggiraph::girafe(ggobj = gg_plot, width_svg = 15)

  #
  # modify girafe object
  #
  gg_girafe <- ggiraph::girafe_options(
    gg_girafe,
    ggiraph::opts_sizing(rescale = TRUE, width = 1.0),
    ggiraph::opts_hover(
      css = "fill-opacity:1;fill:red;stroke:black;",
      reactive = FALSE
    ),
    ggiraph::opts_selection(
      type = c("multiple"),
      only_shiny = TRUE,
      selected = ifelse(skip_selection == TRUE, character(0), selected_items)
    ),
    ggiraph::opts_toolbar(
      position = "topright",
      hidden = c("zoom", "zoomReset", "lasso_deselect"),
      delay_mouseout = 100000
    )

  )
  return(gg_girafe)
}


.analysisResultsHandler_to_studyResults <- function(analysisResults){

  studyResults  <- analysisResults |> dplyr::tbl("timeCodeWASResults")  |>
    dplyr::left_join(
      analysisResults |> dplyr::tbl("timeRef") |>
        dplyr::select(timeId, startDay,endDay),
      by = "timeId"
    ) |>
    dplyr::left_join(
      analysisResults |> dplyr::tbl("covariateRef") ,
      by = "covariateId"
    )|>
    dplyr::left_join(
      analysisResults |> dplyr::tbl("analysisRef") ,
      by = "analysisId"
    )  |>
    dplyr::mutate(
      upIn = dplyr::if_else(oddsRatio>1, "Case", "Ctrl"),
      nCasesInWindow = nCasesInWindow,
      nControlsInWindow = nControlsInWindow
    ) |>
    # dplyr::select(
    #   covariateId, timeId, nCasesYes, nControlsYes, nCases, nControls, nCasesNo, nControlsNo, startDay,endDay,
    #   covariateName, pValue, oddsRatio, upIn
    # ) |>
    dplyr::collect()

  studyResults <- studyResults|>
    dplyr::mutate(timeRange = paste0("from ", as.integer(startDay)," to ", as.integer(endDay)))|>
    dplyr::mutate(oddsRatio = dplyr::if_else(is.na(oddsRatio), Inf, oddsRatio)) |>
    dplyr::rename(
      covariateId = covariateId,
      timeId = timeId,
      timeRange = timeRange,
      covariateName = covariateName,
      p = pValue,
      OR = oddsRatio
    )

  return(studyResults)
}












