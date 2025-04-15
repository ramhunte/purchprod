#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
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

      bslib::layout_columns(
        fill = FALSE,
        uiOutput(ns("year1_box")), # this replaces the static value box,

        bslib::value_box(
          title = "Average bill depth",
          value = "2-19",
          theme = bslib::value_box_theme(bg = "#005E5E"),
          p("The 2nd detail"),
          p("The 3rd detail"),
          showcase = bsicons::bs_icon("filter")
        ),
        bslib::value_box(
          title = "Average body mass",
          value = "2-19",
          theme = bslib::value_box_theme(bg = "#005E5E"),
          p("The 4th detail"),
          p("The 5th detail"),
          p("The 6th detail"),
          showcase = bsicons::bs_icon("currency-dollar")
        )
      ),

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

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$year1_box <- renderUI({
      bslib::value_box(
        title = "Year",
        value = input$year1Input,
        theme = bslib::value_box_theme(bg = "#005E5E"),
        p("The 1st detail"),
        showcase = bsicons::bs_icon("calendar")
      )
    })
  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
