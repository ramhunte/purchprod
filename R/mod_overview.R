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
          theme = bslib::value_box_theme(bg = "#005E5E"),
          showcase = bsicons::bs_icon("calendar")
        ),

        # Total production value
        bslib::value_box(
          title = "Total Production Value",
          value = textOutput(ns("pval_text")),
          theme = bslib::value_box_theme(bg = "#005E5E"),
          showcase = bsicons::bs_icon("currency-dollar")
        ),

        # Change since year 2
        bslib::value_box(
          title = "Difference",
          value = textOutput(ns("diff_text")),
          theme = bslib::value_box_theme(bg = "#005E5E"),
          showcase = bsicons::bs_icon("filter")
        )
      ),

      ######################### Plot Cards ##########################

      bslib::layout_column_wrap(
        width = 1 / 3,
        height = 100,
        bslib::card(
          full_screen = FALSE,
          class = "custom-card",
          bslib::card_header("Year", class = "bg-dark")
        ),
        bslib::card(
          full_screen = FALSE,
          class = "custom-card",
          bslib::card_header("A test plot", class = "bg-dark")
        ),
        bslib::card(
          full_screen = FALSE,
          class = "custom-card",
          bslib::card_header("A filling map", class = "bg-dark"),
          bslib::card_body(class = "p-0")
        )
      ),
      bslib::card(
        full_screen = FALSE,
        class = "custom-card",
        bslib::card_header("A test plot", class = "bg-dark")
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
      clean_purcprod |>
        dplyr::filter(
          year %in% c(input$year1Input, input$year2Input),
          statistic == "Total",
          metric == "Production value",
          variable == "All production"
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

    #   # Year 1 text render
    output$diff_text <- renderText({
      round(
        df()$value[df()$year == input$year1Input] -
          df()$value[df()$year == input$year2Input],
        2
      )
    })
  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
