#' other_tabs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

# this script outline the module that conditionally renders the tab_bottom (lower nav_panel) on the
# lower left hand side of the "Explore the Data" page
# this is paired with a conditional panel in app_server.R
# this renders when tab_top "Summary" or "By Product Type" are selected (not "By Species" which is mod_specs_tabs.R)

mod_other_tabs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # START navset_card_pill
    bslib::navset_card_pill(
      # START nav_panel
      bslib::nav_panel(
        "Production Activities",
        class = "custom-card",
        specs_func(inputID = ns("prodacInput")),
        os_func(inputID1 = ns("osDropdown"), inputID2 = ns("ospsInput"))
      ), # END nav_panel
      # START nav_panel
      bslib::nav_panel(
        "Region",
        class = "custom-card",
        reg_func(inputID = ns("regionInput")),
        pracs_func(inputID = ns("pracs1Input"))
      ), # END nav_panel
      # START nav_panel
      bslib::nav_panel(
        "Processor Size/Type",
        class = "custom-card",
        size_func(inputID = ns("sizeInput")),
        pracs_func(inputID = ns("pracs2Input"))
      ), # END nav_panel
      id = "tab_bottom" # creates an id for the tabs that are used in input$tab_bottom statements for rendering/plotting
    ) # END navset_card_pill
  )
}

#' other_tabs Server Functions
#'
#' @noRd
mod_other_tabs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # return a list of reactive values to be used in the global app_server.R file
    # called in reactive data frames for filtering and plotting
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
