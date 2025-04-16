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
      ######################### Siderbar ##########################
      sidebar = bslib::sidebar(
        # START sidebar
        # Year 1
        year_func(
          inputID = ns("year1Input"),
          label = "Pick Year 1",
          choices = unique(clean_purcprod$year),
          selected = "2023"
        ),
        # Year 2
        year_func(
          inputID = ns("year2Input"),
          label = "Pick Year 2",
          choices = unique(clean_purcprod$year),
          selected = "2022"
        )
      ), #END sidebar

      ######################### Value Boxes ##########################

      # Year 1

      bslib::layout_columns(
        fill = FALSE,
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

        # placeholder vlaue box
        bslib::value_box(
          title = "Placeholder",
          value = "Statistic",
          theme = bslib::value_box_theme(bg = "#90DFE3", fg = "#E9F3F6"),
          showcase = bsicons::bs_icon("calendar")
        ),
      ),

      ######################### Plot Cards ##########################

      # middle cards
      bslib::layout_column_wrap(
        width = 1 / 2,
        height = 200,
        bslib::card(
          full_screen = FALSE,
          class = "custom-card",
          bslib::card_header(
            "Production Value ($ Millions)",
            class = "bg-dark"
          ),
          # plotOutput(ns("specs_bar"))
          plotOutput(ns("specs_bar"))
        ),
        bslib::card(
          full_screen = FALSE,
          class = "custom-card",
          bslib::card_header("A test plot", class = "bg-dark")
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

    df <- reactive({
      sumdf_prac |>
        dplyr::filter(
          year %in% c(input$year1Input, input$year2Input),
          statistic == "Total",
          metric == "Production value",
          variable == "All production"
        )
    })

    # barplot df

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

    # Difference value render
    # output$diff_text <- renderText({
    #   diff <- round(
    #     (df()$value[df()$year == input$year1Input] -
    #       df()$value[df()$year == input$year2Input]) /
    #       df()$value[df()$year == input$year1Input] *
    #       100,
    #     1
    #   )
    #
    #   ifelse(diff > 0, paste0("+", diff), diff)
    # })

    output$diff_text <- renderUI({
      diff <- round(
        (df()$value[df()$year == input$year1Input] -
          df()$value[df()$year == input$year2Input]) /
          df()$value[df()$year == input$year1Input] *
          100,
        1
      )

      color <- if (diff > 0) "green" else "red"
      sign <- if (diff > 0) "+" else ""

      HTML(paste0("<span style='color:", color, "'>", sign, diff, "</span>"))
    })

    # Species barchart
    output$specs_bar <- renderPlot({
      barplot_func(
        data = df_bp(),
        year1 = input$year1,
        year2 = input$year2
      )
    })
  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
