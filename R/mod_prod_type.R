#' prod_type UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_prod_type_ui <- function(id) {
  ns <- NS(id)
  tagList(
    metric_func2(inputID = ns("metricInput")),
    prodtype_func(inputID = ns("protypeInput")),
    stat_func(inputID = ns("statInput"))
  )
}

#' prod_type Server Functions
#'
#' @noRd
mod_prod_type_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_prod_type_ui("prod_type_1")

## To be copied in the server
# mod_prod_type_server("prod_type_1")
