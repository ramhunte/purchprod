#' other_tabs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_other_tabs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Production Activities",
        class = "custom-card",
        specs_func(inputID = ns("prodacInput")),
        os_func(inputID1 = ns("osDropdown"), inputID2 = ns("ospsInput"))
      ),
      bslib::nav_panel(
        "Region",
        class = "custom-card",
        reg_func(inputID = ns("regionInput")),
        pracs_func(inputID = ns("pracs1Input"))
      ),
      bslib::nav_panel(
        "Processor Size/Type",
        class = "custom-card",
        size_func(inputID = ns("sizeInput")),
        pracs_func(inputID = ns("pracs2Input"))
      ),
      id = "tab_bottom"
    )
  )
}

#' other_tabs Server Functions
#'
#' @noRd
mod_other_tabs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(reactive({
      list(
        prodac = input$prodacInput,
        os_dropdown = input$osDropdown,
        osps = input$ospsInput,
        region = input$regionInput,
        pracs1 = input$pracs1Input,
        size = input$sizeInput,
        pracs2 = input$pracs2Input
      )
    }))
  })
}

## To be copied in the UI
# mod_other_tabs_ui("other_tabs_1")

## To be copied in the server
# mod_other_tabs_server("other_tabs_1")
