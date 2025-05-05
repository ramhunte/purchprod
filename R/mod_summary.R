#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

# this script dictates the UI and server of the "Summary" tab on the Explore the Data page
mod_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    metric_func1(inputID = ns("metricInput"))
    # stat_func(inputID = ns("statInput"))
  )
}

#' summary Server Functions
#'
#' @noRd
mod_summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(reactive({
      list(
        metric = input$metricInput
        # stat = input$statInput
      )
    }))
  })
}

## To be copied in the UI
# mod_summary_ui("summary_1")

## To be copied in the server
# mod_summary_server("summary_1")
