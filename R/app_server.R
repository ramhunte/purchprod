#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  ##################### Loading Module Server Outputs #########################
  summary_inputs <- mod_summary_server("summary_1")
  prod_type_inputs <- mod_prod_type_server("prod_type_1")
  specs_inputs <- mod_specs_server("specs_1")
  other_tabs_inputs <- mod_other_tabs_server("other_tabs_1")
  specs_tabs_inputs <- mod_specs_tabs_server("specs_tabs_1")

  ##################### Loading Module Server Outputs #########################
  mod_overview_server("overview_1")

  ##################### Conditional Panel Render in UI #########################

  # conditional panel render if top nav_panels are "Summary" or "By Product Type"
  output$otherTabs <- renderUI({
    mod_other_tabs_ui("other_tabs_1")
  })

  # conditional panel render if top nav_panels is "Species"
  output$speciesTabs <- renderUI({
    mod_specs_tabs_ui("specs_tabs_1")
  })

  ##################### Reactive Summary DF #########################

  # reactive DF for summary tab plot data frame
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
      # "Summary" and "Production Acitviites"
      input$tab_top == "Summary" && input$tab_bottom == "Production Activities"
    ) {
      df <- sumdf_prac |>
        dplyr::filter(
          # top tab filters
          metric %in% summary_inputs()$metric,
          statistic %in% summary_inputs()$stat,
          # bottom tab filters
          variable %in%
            c(other_tabs_inputs()$prodac, other_tabs_inputs()$osps)
        )
    } else if (
      # "Summary" and "Region"
      input$tab_top == "Summary" && input$tab_bottom == "Region"
    ) {
      df <- sumdf_reg |>
        dplyr::filter(
          # top tab filters
          metric %in% summary_inputs()$metric,
          statistic %in% summary_inputs()$stat,
          # bottom tab filters
          variable %in% other_tabs_inputs()$reg,
          cs %in% other_tabs_inputs()$pracs1
        )
    } else if (
      # "Summary" and "Processor Size/Type"
      input$tab_top == "Summary" && input$tab_bottom == "Processor Size/Type"
    ) {
      df <- sumdf_size |>
        dplyr::filter(
          # top tab filters
          metric %in% summary_inputs()$metric,
          statistic %in% summary_inputs()$stat,
          # bottom tab filters
          variable %in% other_tabs_inputs()$size,
          cs %in% other_tabs_inputs()$pracs2
        )
    }

    return(df)
  })

  ##################### Reactive Summary Plots #########################

  # plot for summary tab selections
  output$sumplot <- renderPlot(
    {
      # run function to create plot with summary tab data
      plot_func(
        data = sum_plot_df(),
        lab = summary_inputs()$stat,
        group = "variable",
        facet = "unit_lab"
      )
    }
  )

  ##################### Reactive By Product Type DF ########################### ----

  # reactive data frame for By Product Type tab
  prod_plot_df <- reactive({
    # "By Product Type" and "Production Acitivities"
    if (input$tab_top == "By Product Type") {
      if (input$tab_bottom == "Production Activities") {
        df <- proddf_prac |>
          dplyr::filter(
            # top tab filters
            metric %in% prod_type_inputs()$metric,
            type %in% prod_type_inputs()$prod_type,
            statistic == prod_type_inputs()$stat,
            # bottom tab filters
            variable %in%
              c(other_tabs_inputs()$prodac, other_tabs_inputs()$osps)
          )
      } else if (
        # "By Product Type" and "Region"
        input$tab_bottom == "Region"
      ) {
        df <- proddf_reg |>
          dplyr::filter(
            # top tab filters
            metric %in% prod_type_inputs()$metric,
            type %in% prod_type_inputs()$prod_type,
            statistic %in% prod_type_inputs()$stat,
            # bottom tab filters
            variable %in% other_tabs_inputs()$reg,
            cs %in% other_tabs_inputs()$pracs1
          )
      } else if (
        # "By Product Type" and "Processor Size/Type"
        input$tab_bottom == "Processor Size/Type"
      ) {
        df <- proddf_size |>
          dplyr::filter(
            # to tab filters
            metric %in% prod_type_inputs()$metric,
            type %in% prod_type_inputs()$prod_type,
            statistic == prod_type_inputs()$stat,
            # bottom tab filters
            variable %in% other_tabs_inputs()$size,
            cs %in% other_tabs_inputs()$pracs2
          )
      }

      df # Return the filtered data frame
    }
  })

  ##################### Reactive By Product Type Plot ###########################

  ##Plot for the product tab
  output$productplot <- renderPlot(
    {
      # run function to create plot with summary tab data
      plot_func(
        data = prod_plot_df(),
        lab = prod_type_inputs()$stat,
        group = "variable",
        facet = "unit_lab"
      )
    }
  )

  ##################### Reactive Species DF ########################### ----

  # reactive data frame for By Product Type tab
  specs_plot_df <- reactive({
    if (input$tab_top == "By Species") {
      df <- specsdf |>
        dplyr::filter(
          # top tab filters
          metric %in% specs_inputs()$metric,
          variable %in% c(specs_inputs()$specs, specs_inputs()$os),
          statistic == specs_inputs()$stat,
          # bottom tab filters
          type %in% specs_tabs_inputs()$prodtype
        )
      return(df)
    }

    return(NULL) # If tab_bottom is not valid
  })

  ##################### Reactive Species tab Plot ###########################

  ##Plot for the species tab
  output$specsplot <- renderPlot(
    {
      # run function to create plot with summary tab data
      plot_func(
        data = specs_plot_df(),
        lab = specs_inputs()$stat,
        group = "type",
        facet = "unit_lab",
      )
    }
  )

  ##################### Reactive Data Table  #########################

  ##Creating the data table
  output$table <- DT::renderDT(
    {
      if (input$tab_top == "Summary") {
        df <- sum_plot_df()
      } else if (input$tab_top == "By Product Type") {
        df <- prod_plot_df()
      } else if (input$tab_top == "By Species") {
        df <- specs_plot_df()
      }

      # Process the data using the helper function
      process_df(df)
    },
    options = list(
      scrollX = TRUE, # Enable horizontal scroll
      scrollY = "680px" # setting vertical scroll height
    )
  )

  ##################### Data Table Download #########################

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("purchprod", input$tab_top, "data.csv", sep = "_")
    },
    content = function(file) {
      if (input$tab_top == "Summary") {
        write.csv(sum_plot_df(), file)
      } else if (input$tab_top == "By Product Type") {
        write.csv(prod_plot_df(), file)
      } else if (input$tab_top == "By Product Type") {
        write.csv(specs_plot_df(), file)
      }
    }
  )

  observe({
    print(paste("Current tab:", input$tab_bottom))
  })
}
