#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  observeEvent(input$tab_top, {
    if (input$tab_top != "Species") {
      updateTabsetPanel(
        session,
        "tab_bottom",
        selected = "Production Activities"
      )
    }
  })

  # loading in module server outputs
  summary_inputs <- mod_summary_server("summary_1")
  prod_type_inputs <- mod_prod_type_server("prod_type_1")
  specs_inputs <- mod_specs_server("specs_1")
  other_tabs_inputs <- mod_other_tabs_server("other_tabs_1")
  specs_tabs_inputs <- mod_specs_tabs_server("specs_tabs_1")

  ##################### Reactive Summary DF #########################

  sum_plot_df <- reactive({
    req(input$tab_top == "Summary")
    req(
      input$tab_bottom %in%
        c("Production Activities", "Region", "Processor Size/Type")
    )
    df <- NULL

    if (
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
    } else if (input$tab_top == "Summary" && input$tab_bottom == "Region") {
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

  output$sumplot <- renderPlot(
    {
      req(sum_plot_df())

      # # creating a validatement statement requiring valid inputs
      validate(
        need(sum_plot_df(), "No data available for these selected inputs"),
        need(
          nrow(sum_plot_df()) > 0,
          "No data available for these selected inputs"
        )
      )
      # run function to create plot with summary tab data
      plot_func(
        data = sum_plot_df(),
        lab = "test",
        group = "variable",
        facet = "unit_lab"
      )
    }
  )

  ##################### Reactive By Product Type DF ########################### ----

  # reactive data frame for By Product Type tab
  prod_plot_df <- reactive({
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
      } else if (input$tab_bottom == "Region") {
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
      } else if (input$tab_bottom == "Processor Size/Type") {
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
      validate(
        need(prod_plot_df(), "No data available for these selected inputs"),
        need(
          nrow(prod_plot_df()) > 0,
          "No data available for these selected inputs"
        )
      )

      # run function to create plot with summary tab data
      plot_func(
        data = prod_plot_df(),
        lab = input$stat2Input,
        group = "variable",
        facet = "unit_lab"
      )
    }
  )

  ##################### Reactive Species DF ########################### ----

  # reactive data frame for By Product Type tab
  specs_plot_df <- reactive({
    if (input$tab_top == "Species") {
      df <- proddf_prac |>
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
      validate(
        need(specs_plot_df(), "No data available for these selected inputs"),
        need(
          nrow(specs_plot_df()) > 0,
          "No data available for these selected inputs"
        )
      )

      # run function to create plot with summary tab data
      plot_func(
        data = specs_plot_df(),
        lab = input$stat3Input,
        group = "type",
        facet = "variable"
      )
    }
  )

  ##################### Reactive Data Table  #########################

  ##Creating the data table
  output$table <- DT::renderDataTable(
    {
      if (input$tab_top == "Summary") {
        df <- sum_plot_df()
      } else if (input$tab_top == "By Product Type") {
        df <- prod_plot_df()
      } else if (input$tab_top == "Species") {
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
}
