#' @title Cohort TimeCodeWAS Visualization UI
#' @description UI module for visualizing cohort overlaps using an UpSet plot. This module provides controls to customize the appearance of the plot and options to download the plot and data.
#'
#' @param id A string representing the module's namespace.
#'
#' @return A Shiny UI element that can be included in a Shiny app.
#'
#' @importFrom shiny NS fluidPage div fluidRow column actionButton checkboxInput plotOutput downloadButton observeEvent
#' @importFrom shinyWidgets sliderInput chooseSliderSkin
#' @importFrom shinyjs useShinyjs toggle hidden
#' @importFrom htmltools tagList
#' @importFrom shinybrowser detect
#'
#' @export
#'
mod_resultsVisualisation_TimeCodeWAS_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$h4("Filters"),
    shiny::uiOutput(ns("outputUI")),
    shiny::tags$h4("Data"),
    shiny::tabsetPanel(
      id = ns("tabset"),
      shiny::tabPanel(
        "Plot",
        shiny::div(style = "height: 12px;"),
        ggiraph::girafeOutput(ns("codeWASplot"), width = "100%", height = "100%"),
        shiny::div(
          style = "margin-top: 10px; margin-bottom: 10px;",
          shiny::downloadButton(ns("downloadPlot"), "Download")
        )
      ),
      shiny::tabPanel(
        "Table",
        shiny::div(
          style = "margin-top: 10px; margin-bottom: 10px;",
          DT::DTOutput(ns("demographicsData")),
        ),
        shiny::div(
          style = "margin-top: 10px; margin-bottom: 10px;",
          shiny::downloadButton(ns("downloadData"), "Download")
        )
      )
    ), # tabsetPanel
  )


}


#' @title Cohort Demographics Visualization Server
#' @description Server module for handling the logic of the cohort overlaps visualization UI. This module creates an UpSet plot based on the analysis results and allows the plot and data to be downloaded.
#'
#' @param id A string representing the module's namespace.
#' @param analysisResults Pooled connection to the analisys results duckdb.
#'
#' @return The module returns server-side logic to generate and manage the cohort overlaps UpSet plot.
#'
#' @importFrom shiny moduleServer reactive req renderPlot downloadHandler
#' @importFrom shinyjs toggle
#' @importFrom dplyr tbl collect mutate
#' @importFrom UpSetR upset fromExpression
#' @importFrom grDevices cairo_pdf dev.off
#'
#' @export
#'
mod_resultsVisualisation_TimeCodeWAS_server <- function(id, analysisResults) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    atlasUrl <- "TEMP" #shiny::getShinyOption("cohortOperationsConfig")$atlasUrl

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
        shiny::fluidRow(
          shiny::column(
            3,
            shinyWidgets::pickerInput(
              ns("selected_domains"),
              "Observation type",
              choices = c(
                "Condition occurrence" = "condition_occurrence",
                "Drug exposure" = "drug_exposure",
                "Measurement" = "measurement",
                "Procedure occurrence" = "procedure_occurrence",
                "Observation" = "observation"
              ),
              selected = c("condition_occurrence", "drug_exposure", "measurement", "procedure_occurrence", "observation"),
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                selectedTextFormat = 'count',
                countSelectedText = '{0} observation types selected'
              ),
              multiple = TRUE
            )
          ),
          shiny::column(
            3,
            shinyWidgets::pickerInput(
              ns("selected_p_groups"),
              "p-value groups",
              choices = c(
                "-log10(p) [0,50]" = 1,
                "-log10(p) (50,100]" = 5,
                "-log10(p) (100,200]" = 10,
                "-log10(p) (200,Inf]" = 20
                ),
              selected = c(1, 5, 10, 20),
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                selectedTextFormat = 'count',
                countSelectedText = '{0} p-value groups selected'
              ),
              multiple = TRUE
            ),
          )
        )
      )
    })

    #
    # observe the selected domains and p_groups
    #
    shiny::observe({
      shiny::req(input$selected_domains)
      shiny::req(input$selected_p_groups)

      # filter data
      gg_data <- gg_data_saved |>
        dplyr::filter(domain %in% input$selected_domains) |>
        dplyr::filter(p_group_size %in% input$selected_p_groups)

      # update gg_data
      r$gg_data <- gg_data
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
        r = r
      )

      return(gg_girafe)
      # replotting this triggers codeWASplot_selected
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

      line_to_plot <- NULL

      if(length(selected_rows) > 1){
        # we have a marquee selection with n > 1
        df_lasso <- r$gg_data |>
          dplyr::filter(data_id %in% selected_rows) |>
          dplyr::mutate(cases_per = scales::percent(cases_per, accuracy = 0.01)) |>
          dplyr::mutate(controls_per = scales::percent(controls_per, accuracy = 0.01)) |>
          dplyr::mutate(p = as.numeric(formatC(p, format = "e", digits = 2))) |>
          dplyr::select(name, upIn, nCasesYes, nControlsYes, cases_per, controls_per, GROUP, p)

        # show table
        shiny::showModal(
          shiny::modalDialog(
            shiny::div(
            tags$style(HTML(".modal-dialog {width: 90%; max-width: 90%;}")),
            DT::renderDataTable({
              DT::datatable(
                df_lasso,
                colnames = c(
                  # 'Covariate ID' = 'code',
                  'Covariate name' = 'name',
                  'Type' = 'upIn',
                  'Cases n' = 'nCasesYes',
                  'Ctrls n' = 'nControlsYes',
                  'Cases %' = 'cases_per',
                  'Ctrls %' = 'controls_per',
                  'Group' = 'GROUP',
                  'p' = 'p'
                ),
                options = list(
                  formatter = list(
                    p = function(x) format(x, scientific = TRUE)
                  )
                )
              )
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
    # show the data as a table ####
    #
    output$demographicsData <- DT::renderDataTable({
      shiny::req(r$gg_data)

      df_all <- r$gg_data |>
        dplyr::mutate(cases_per = scales::percent(cases_per, accuracy = 0.01)) |>
        dplyr::mutate(controls_per = scales::percent(controls_per, accuracy = 0.01))|>
        dplyr::mutate(
          code = round(code/1000),
          name = purrr::map2_chr(name, code, ~paste0('<a href="',atlasUrl,'/#/concept/', .y, '" target="_blank">', .x,'</a>'))
        ) |>
        dplyr::select(name, upIn, OR, nCasesYes, nControlsYes, cases_per, controls_per, GROUP, p)

      # show table
      df_all |>
        DT::datatable(
          colnames = c(
            'Covariate name' = 'name',
            'Type' = 'upIn',
            'OR' = 'OR',
            'Cases n' = 'nCasesYes',
            'Ctrls n' = 'nControlsYes',
            'Cases %' = 'cases_per',
            'Ctrls %' = 'controls_per',
            'Group' = 'GROUP',
            'p' = 'p'
          ),
          options = list(
            order = list(list(9, 'asc'))
          ),
          escape = FALSE,
        ) |>
        DT::formatSignif(columns = c('p', 'OR'), digits = 3) |>
        DT::formatStyle('Covariate name', cursor = 'pointer' )
    })

    #
    # download data as a table
    #
    output$downloadData <- shiny::downloadHandler(
      filename = function(){
        paste('timecodewas_', format(lubridate::now(), "%Y_%m_%d_%H%M"), '.csv', sep='')
      },
      content = function(fname){
        readr::write_csv(r$gg_data |> dplyr::select(-label), fname)
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

  l <- unique(studyResult$timeRange)
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
      OR=OR,
      p=p,
      upIn=upIn,
      cases_per = nCasesYes/nCases,
      controls_per = nControlsYes/nControls,
      nCasesYes = nCasesYes,
      nControlsYes = nControlsYes
    ) |>
    tidyr::separate(name, c("domain", "name"), sep = ":", extra = "merge") |>
    dplyr::mutate(name = stringr::str_remove(name, "^[:blank:]")) |>
    dplyr::mutate(p = dplyr::if_else(p==0, 10^-323, p))

  gg_data <- studyResult |>
    dplyr::filter(p<0.00001) |>
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
    r
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
      #
      ggrepel::geom_text_repel(
        data =  gg_data |>
          dplyr::filter(data_id %in% selection$data_id),
        ggplot2::aes(label = stringr::str_wrap(stringr::str_trunc(name, 30), 15)),
        max.overlaps = Inf,
        size = 3,
        hjust = 0.1,
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
      legend.key.height = grid::unit(5, "mm"),
      legend.key.width = grid::unit(10, "mm"),
      legend.position = "bottom",
      legend.direction = "vertical",
      strip.text.x = ggplot2::element_text(size = 10)
    ) +
    ggplot2::scale_color_manual(values = c("darkgray")) +
    ggplot2::scale_fill_manual(values = c(
      "condition_occurrence" = "khaki",
      "drug_exposure" = "lightblue2",
      "measurement" = "palegreen",
      "procedure_occurrence" = "plum1",
      "observation" = "orange"),
      labels = c(
        "condition_occurrence" = "Condition occurrence",
        "drug_exposure" = "Drug exposure",
        "measurement" ="Measurement",
        "observation" = "Observation"
      )
    ) +
    ggplot2::guides(color = "none", fill = ggplot2::guide_legend(override.aes = list(size = 5))) +
    ggplot2::labs(size = "p value group", fill = "Domain", x = "\nControls %", y = "Cases %")

  r$gg_plot <- gg_fig

  selected_items <- ""

  if(!is.null(selection) && length(unique(selection$code)) == 1){
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
      nCases = nCasesYes + nCasesNo,
      nControls = nControlsYes + nControlsNo
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












