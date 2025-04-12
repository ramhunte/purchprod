#' specs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_specs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    metric_func2(inputID = ns("metricInput")),
    specs_func(inputID = "specsInput"),
    os_func(inputID1 = "osDropdown", inputID2 = "ospsInput"),
    stat_func(inputID = "statInput")
  )
}

#' specs Server Functions
#'
#' @noRd
mod_specs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_specs_ui("specs_1")

## To be copied in the server
# mod_specs_server("specs_1")
