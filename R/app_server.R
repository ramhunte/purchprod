#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #################################################################################
  #################################### Overview ###################################
  #################################################################################

  # Loading Module Server Outputs
  mod_overview_server("overview_1")

  #################################################################################
  ################################ Explore the Data ###############################
  #################################################################################

  # NOTE: so much of the "Explore the Data" tab's server is in this app_server.R and not
  # the module_*_.R servers because the inputs for the graphs/tables/data frames come
  # from various modules. Therefore, components of each of the modules themselves all
  # feed into global server file (app_server.R) components

  # Loading Module Server Outputs

  # top tabs
  summary_inputs <- mod_summary_server("summary_1")
  prod_type_inputs <- mod_prod_type_server("prod_type_1")
  specs_inputs <- mod_specs_server("specs_1")

  # bottom tabs
  other_tabs_inputs <- mod_other_tabs_server("other_tabs_1")
  specs_tabs_inputs <- mod_specs_tabs_server("specs_tabs_1")

  # output for conditional bottom tabs

  # if top nav_panels are "Summary" or "By Product Type", then "other_tabs_1"
  output$otherTabs <- renderUI({
    mod_other_tabs_ui("other_tabs_1")
  })

  # if top nav_panels are "By Species", then "specs_tabs_1"
  output$speciesTabs <- renderUI({
    mod_specs_tabs_ui("specs_tabs_1")
  })

  ############################# Deflator value  #################################

  defl_val <- reactive({
    gdp_defl |>
      dplyr::filter(.data$YEAR == input$deflInput) |>
      dplyr::pull(.data$DEFL)
  })

  ############################# "Summary" tab: reactive data frame  #################################

  sum_plot_df <- reactive({
    req(
      input$tab_top == "Summary",
      input$tab_bottom %in%
        c("Production Activities", "Region", "Processor Size/Type"),
      summary_inputs(),
      other_tabs_inputs()
    )

    # Conditional rendering depending on the tab_bottom select
    if (
      # "Summary" and "Production Activities"
      input$tab_top == "Summary" && input$tab_bottom == "Production Activities"
    ) {
      df <- sumdf_prac |>
        dplyr::filter(
          # "Summary" tab filters
          .data$metric %in% summary_inputs()$metric,
          # .data$statistic %in% summary_inputs()$stat,
          # "Production Activities"
          .data$variable %in%
            c(other_tabs_inputs()$prodac, other_tabs_inputs()$osps)
        ) |>
        dplyr::mutate(
          # adjusting price realted cols for deflation value
          value = dplyr::case_when(
            .data$metric %in%
              c(
                "Markup",
                "Production price (per lb)",
                "Production value",
                "Purchase price (per lb)",
                "Purchase value"
              ) ~
              .data$value * defl_val() / .data$defl,

            # for metrics that dont have price involved
            TRUE ~ .data$value
          )
        )
    } else if (
      # "Summary" and "Region"
      input$tab_top == "Summary" && input$tab_bottom == "Region"
    ) {
      df <- sumdf_reg |>
        dplyr::filter(
          # "Summary" tab filters
          .data$metric %in% summary_inputs()$metric,
          # .data$statistic %in% summary_inputs()$stat,
          # "Region" tab filters
          .data$variable %in% other_tabs_inputs()$reg,
          .data$cs %in% other_tabs_inputs()$pracs1
        ) |>
        dplyr::mutate(
          # adjusting price realted cols for deflation value
          value = dplyr::case_when(
            .data$metric %in%
              c(
                "Markup",
                "Production price (per lb)",
                "Production value",
                "Purchase price (per lb)",
                "Purchase value"
              ) ~
              .data$value * defl_val() / .data$defl,

            # for metrics that dont have price involved
            TRUE ~ .data$value
          )
        )
    } else if (
      # "Summary" and "Processor Size/Type"
      input$tab_top == "Summary" && input$tab_bottom == "Processor Size/Type"
    ) {
      df <- sumdf_size |>
        dplyr::filter(
          # "Summary" tab filters
          .data$metric %in% summary_inputs()$metric,
          # .data$statistic %in% summary_inputs()$stat,
          # "Processor Size/Type" tab filters
          .data$variable %in% other_tabs_inputs()$size,
          .data$cs %in% other_tabs_inputs()$pracs2
        ) |>
        dplyr::mutate(
          # adjusting price realted cols for deflation value
          value = dplyr::case_when(
            .data$metric %in%
              c(
                "Markup",
                "Production price (per lb)",
                "Production value",
                "Purchase price (per lb)",
                "Purchase value"
              ) ~
              .data$value * defl_val() / .data$defl,

            # for metrics that dont have price involved
            TRUE ~ .data$value
          )
        )
    }

    return(df)
  })

  ############################ "By Product Type" tab: reactive data frame  ###############################

  # reactive data frame for "By Product Type" tab
  prod_plot_df <- reactive({
    # "By Product Type" and "Production Acitivities"
    if (input$tab_top == "By Product Type") {
      if (input$tab_bottom == "Production Activities") {
        df <- proddf_prac |>
          dplyr::filter(
            # "By Product Type" tab filters
            .data$metric %in% prod_type_inputs()$metric,
            .data$type %in% prod_type_inputs()$prod_type,
            # .data$statistic == prod_type_inputs()$stat,
            # "Production Activities" tab filters
            .data$variable %in%
              c(other_tabs_inputs()$prodac, other_tabs_inputs()$osps)
          ) |>
          dplyr::mutate(
            # adjusting price realted cols for deflation value
            value = dplyr::case_when(
              .data$metric %in%
                c(
                  "Markup",
                  "Production price (per lb)",
                  "Production value",
                  "Purchase price (per lb)",
                  "Purchase value"
                ) ~
                .data$value * defl_val() / .data$defl,

              # for metrics that dont have price involved
              TRUE ~ .data$value
            )
          )
      } else if (
        # "By Product Type" and "Region"
        input$tab_bottom == "Region"
      ) {
        df <- proddf_reg |>
          dplyr::filter(
            # "By Product Type" tab filters
            .data$metric %in% prod_type_inputs()$metric,
            .data$type %in% prod_type_inputs()$prod_type,
            # .data$statistic %in% prod_type_inputs()$stat,
            # "Region" tab filters
            .data$variable %in% other_tabs_inputs()$reg,
            .data$cs %in% other_tabs_inputs()$pracs1
          ) |>
          dplyr::mutate(
            # adjusting price realted cols for deflation value
            value = dplyr::case_when(
              .data$metric %in%
                c(
                  "Markup",
                  "Production price (per lb)",
                  "Production value",
                  "Purchase price (per lb)",
                  "Purchase value"
                ) ~
                .data$value * defl_val() / .data$defl,

              # for metrics that dont have price involved
              TRUE ~ .data$value
            )
          )
      } else if (
        # "By Product Type" and "Processor Size/Type"
        input$tab_bottom == "Processor Size/Type"
      ) {
        df <- proddf_size |>
          dplyr::filter(
            # "By Product Type" tab filters
            .data$metric %in% prod_type_inputs()$metric,
            .data$type %in% prod_type_inputs()$prod_type,
            # .data$statistic == prod_type_inputs()$stat,
            # "Processor Size/Type" tab filters
            .data$variable %in% other_tabs_inputs()$size,
            .data$cs %in% other_tabs_inputs()$pracs2
          ) |>
          dplyr::mutate(
            # adjusting price realted cols for deflation value
            value = dplyr::case_when(
              .data$metric %in%
                c(
                  "Markup",
                  "Production price (per lb)",
                  "Production value",
                  "Purchase price (per lb)",
                  "Purchase value"
                ) ~
                .data$value * defl_val() / .data$defl,

              # for metrics that dont have price involved
              TRUE ~ .data$value
            )
          )
      }

      df # Return the filtered data frame
    }
  })

  ########################### "By Species" tab: reactive data frame #################################

  specs_plot_df <- reactive({
    # only one option here b/c not multiple tabs on tab_bottom panel. Just 1
    if (input$tab_top == "By Species") {
      df <- specsdf |>
        dplyr::filter(
          # "By Species" tab filters
          .data$metric %in% specs_inputs()$metric,
          .data$variable %in% c(specs_inputs()$specs, specs_inputs()$os),
          # bottom tab filters
          .data$type %in% specs_tabs_inputs()$prodtype
        ) |>
        dplyr::mutate(
          # adjusting price realted cols for deflation value
          value = dplyr::case_when(
            .data$metric %in%
              c(
                "Markup",
                "Production price (per lb)",
                "Production value",
                "Purchase price (per lb)",
                "Purchase value"
              ) ~
              .data$value * defl_val() / .data$defl,

            # for metrics that dont have price involved
            TRUE ~ .data$value
          )
        )
      return(df)
    }

    return(NULL) # If tab_bottom is not valid
  })

  ############################################ Plots  ############################################

  # this chunk renders the the main panel "Plot" tab plot in the UI
  # conditional render depending on which tab_top is selected

  output$exp_plot_ui <- renderPlot({
    # "Summary" tab plot
    if (input$tab_top == "Summary") {
      plot_func(
        # plot function
        data = sum_plot_df(), # "Summary" tab reactive data frame
        # lab = summary_inputs()$stat, # using the statistic as label
        lab = NULL,
        group = "variable", # grouping by variable
        facet = "unit_lab" # faceting by unit label
        # )
      )
      # "By Product Type" tab plot
    } else if (input$tab_top == "By Product Type") {
      plot_func(
        data = prod_plot_df(), # same steps as above for "Summary" ^^
        # lab = prod_type_inputs()$stat,
        lab = NULL,
        group = "variable",
        facet = "unit_lab"
        # )
      )
      # "By Species" tab plot
    } else if (input$tab_top == "By Species") {
      plot_func(
        data = specs_plot_df(),
        lab = NULL,
        # lab = specs_inputs()$stat,
        group = "type", # faceting by product type (different column than variable for previous examples ^)
        facet = "unit_lab",
      )
    }
  })

  ############################################ Table  ############################################

  # this chunk renders the the main panel "Table" tab table in the UI
  # conditional render depending on which tab_top is selected
  output$table <- DT::renderDT(
    {
      if (input$tab_top == "Summary") {
        df <- sum_plot_df() #render "Summary" tab table
      } else if (input$tab_top == "By Product Type") {
        df <- prod_plot_df() #render "By Product Type" tab table
      } else if (input$tab_top == "By Species") {
        df <- specs_plot_df() #render "By Species" tab table
      }

      # Process the data using the function to make it pretty for the table
      process_df(df)
    },
    options = list(
      scrollX = TRUE, # Enable horizontal scroll
      scrollY = "680px" # setting vertical scroll height
    )
  )

  ####################################### Data Table Download #######################################

  # download data button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("purchprod", input$tab_top, "data.csv", sep = "_") # title for csv
    },
    content = function(file) {
      # conditional table render tdepnding on tab_top selection
      if (input$tab_top == "Summary") {
        utils::write.csv(sum_plot_df(), file)
      } else if (input$tab_top == "By Product Type") {
        utils::write.csv(prod_plot_df(), file)
      } else if (input$tab_top == "By Species") {
        utils::write.csv(specs_plot_df(), file)
      }
    }
  )
}
