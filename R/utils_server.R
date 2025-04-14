#' server
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

########################### summary data frame ###############################

sum_plot_df <- reactive({
  req(input$tab_top == "Summary")
  req(input$tab_bottom)

  if (input$tab_top == "Summary") {
    if (input$tab_bottom == "Production Activities") {
      df <- sumdf_prac |>
        filter(
          metric %in% summary_inputs$metric(),
          statistic == summary_inputs$stat(),
          variable %in% c(input$prodacInput, input$ospsInput)
        )
    }
    # else if (input$tab_bottom == "Region") {
    #   df <- sumdf_reg |>
    #     filter(
    #       metric %in% input$metricInput,
    #       statistic == input$statInput,
    #       variable %in% input$regionInput,
    #       cs %in% input$pracs1Input
    #     )
    # } else if (input$tab_bottom == "Processor Size/Type") {
    #   df <- sumdf_size |>
    #     filter(
    #       metric %in% input$metricInput,
    #       statistic == input$statInput,
    #       variable %in% input$sizeInput,
    #       cs %in% input$pracs2Input
    #     )
    # }

    df # Return the filtered data frame
  }
})
