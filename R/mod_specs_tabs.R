#' specs_tabs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_specs_tabs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Product Type",
        class = "custom-card",
        prodtype_func(inputID = ns("protype2Input"))
      )
    )
  )
}

#' specs_tabs Server Functions
#'
#' @noRd
mod_specs_tabs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(reactive({
      list(
        prodtype = input$protype2Input
      )
    }))
  })
}

## To be copied in the UI
# mod_specs_tabs_ui("specs_tabs_1")

## To be copied in the server
# mod_specs_tabs_server("specs_tabs_1")
