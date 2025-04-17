#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

######################### UI ##########################

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      ######################### Year Selectors #########################
      sidebar = bslib::sidebar(
        title = "Select years to compare",
        year_func(
          inputID = ns("year1Input"),
          label = "Year 1",
          choices = unique(clean_purcprod$year),
          selected = "2023"
        ),
        year_func(
          inputID = ns("year2Input"),
          label = "Year 2",
          choices = unique(clean_purcprod$year),
          selected = "2022"
        )
      ),
      bslib::layout_columns(
        fill = FALSE,

        ######################### Value Boxes #########################
        bslib::value_box(
          title = "Year",
          value = textOutput(ns("year1_text")),
          theme = bslib::value_box_theme(bg = "#056FB7", fg = "#E9F3F6"),
          showcase = bsicons::bs_icon("calendar")
        ),

        # Total production value
        bslib::value_box(
          title = "Total Production Value",
          value = textOutput(ns("pval_text")),
          theme = bslib::value_box_theme(bg = "#5EB6D9", fg = "#E9F3F6"),
          showcase = bsicons::bs_icon("currency-dollar")
        ),

        # Change since year 2
        bslib::value_box(
          title = textOutput(ns("value_box_title")),
          value = uiOutput(ns("diff_text")),
          theme = bslib::value_box_theme(bg = "#1EBEC7", fg = "#E9F3F6"),
          showcase = bsicons::bs_icon("percent")
        ),

        # number of observations
        bslib::value_box(
          title = "Number of observations",
          value = textOutput(ns("n_text")),
          theme = bslib::value_box_theme(bg = "#90DFE3", fg = "#E9F3F6"),
          showcase = bsicons::bs_icon("hash")
        ),
      ),

      ######################### Plot Cards ##########################

      # middle cards
      bslib::layout_column_wrap(
        width = 1 / 2,
        height = 200,

        # species barplot
        bslib::card(
          full_screen = TRUE,
          class = "custom-card p-0",
          bslib::card_header(
            "Production Value ($ Millions)",
            class = "bg-dark"
          ),

          bslib::card_body(
            plotOutput(ns("specs_bar")),
            class = "p-0" # remove padding
          )
        ),

        # other plot
        bslib::card(
          full_screen = TRUE,
          class = "custom-card",
          bslib::card_header("A test plot", class = "bg-dark"),
          plotOutput(ns("tree_plot"))
        )
      )
    )
  )
}


######################### Server ##########################

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # overall data frame
    df <- reactive({
      sumdf_prac |>
        dplyr::filter(
          year %in% c(input$year1Input, input$year2Input),
          statistic == "Total",
          metric == "Production value",
          variable == "All production"
        )
    })

    # barplot data frame

    df_bp <- reactive({
      sumdf_prac |>
        dplyr::filter(
          year %in% c(input$year1Input, input$year2Input),
          statistic == "Total",
          metric == "Production value"
        )
    })

    # Year 1 text render
    output$year1_text <- renderText({
      input$year1Input
    })

    # Production Value Text
    output$pval_text <- renderText({
      df() |>
        dplyr::filter(year == input$year1Input) |>
        dplyr::pull(value) |>
        round(2) |>
        paste("M")
    })

    # Difference title render
    output$value_box_title <- renderText({
      paste0("Change since ", input$year2Input) # or whatever reactive source you're using
    })

    # Difference change value
    output$diff_text <- renderUI({
      # calculating % change
      diff <- round(
        (df()$value[df()$year == input$year1Input] -
          df()$value[df()$year == input$year2Input]) /
          df()$value[df()$year == input$year1Input] *
          100,
        1
      )

      # assigning colros and +/- sign to value
      color <- if (diff > 0) "#4B8320" else "#DB2207"
      sign <- if (diff > 0) "+" else ""

      # HTML to change colors depending on value
      HTML(paste0("<span style='color:", color, "'>", sign, diff, "</span>"))
    })

    # number of observations output
    output$n_text <- renderText({
      df()$n[df()$year == input$year1Input]
    })

    ################# Plots ####################

    # Species barchart
    output$specs_bar <- renderPlot({
      lollipop_func(
        data = df_bp(),
        year1 = input$year1,
        year2 = input$year2
      )
    })

    # tree plot
    output$tree_plot <- renderPlot({
      tree_fig # your static ggplot object
    })
  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
