#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # conditional panel render if top nav_panels are "Summary" or "By Product Type"
  output$otherTabs <- renderUI({
    other_tabs_func()
  })

  # conditional panel render if top nav_panels is "Species"
  output$speciesTabs <- renderUI({
    species_tabs_func()
  })
}
