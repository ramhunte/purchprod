#' specs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

# this script dictates the UI and server of the "By Species" tab on the Explore the Data page

mod_specs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    metric_func2(inputID = ns("metricInput")),
    specs_func(inputID = ns("specsInput")),
    os_func(inputID1 = ns("osDropdown"), inputID2 = ns("ospsInput"))
    # stat_func(inputID = ns("statInput"))
  )
}

#' specs Server Functions
#'
#' @noRd
mod_specs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(reactive({
      list(
        metric = input$metricInput,
        specs = input$specsInput,
        os = input$ospsInput
        # stat = input$statInput
      )
    }))
  })
}

## To be copied in the UI
# mod_specs_ui("specs_1")

## To be copied in the server
# mod_specs_server("specs_1")
