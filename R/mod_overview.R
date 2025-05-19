#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

# this script dictates the UI and server of the Overview landing page

######################################################################################
####################################### UI ###########################################
######################################################################################

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        width = "19%",
        # START  div
        tags$div(
          style = "margin-bottom: 20px;",
          # Select a year:
          year_func(
            inputID = ns("year1Input"),
            label = bslib::tooltip(
              span(
                strong(em("Select a year")),
                bsicons::bs_icon("info-circle")
              ),
              "Select a year in the dropdown (e.g. 2023) you would like to analyze.",
              options = list(container = "body"), # optional: avoids overflow issues
              style = "position: absolute; top: 10px; right: 10px; cursor: pointer;"
            ),
            choices = unique(sumdf_prac$year),
            selected = "2023",
            options = list(`style` = "btn-year1")
          )
        ), # END div()

        # Select a date range:

        # START  div
        tags$div(
          style = "margin-bottom: 20px;",
          year_range_func(
            inputID = ns("yearrangeInput"),
            label = bslib::tooltip(
              span(
                strong(em("Select a date range or year")),
                bsicons::bs_icon("info-circle")
              ),
              "Select a year range (e.g. 2015-2020) to get an average total production value across years OR select a single year (slide both ends of the range onto the same year). This value is compared
              to the previous year selected in the dropdown above.",
              options = list(container = "body"), # optional: avoids overflow issues
              style = "position: absolute; top: 10px; right: 10px; cursor: pointer;"
            ),
            min = min(sumdf_prac$year),
            max = max(sumdf_prac$year),
            value = c(2015, 2020)
          )
        ), # END div

        # Select a region or processor size
        # START  div
        tags$div(
          style = "margin-bottom: 20px;",
          radioButtons(
            inputId = ns("regsizeInput"),
            label = bslib::tooltip(
              span(
                strong(em("Select processor size or state")),
                bsicons::bs_icon("info-circle")
              ),
              "Filter the data to processor size (Small/Medium or Large) or by region (California or Washington and Oregon)",
              options = list(container = "body"), # optional: avoids overflow issues
              style = "position: absolute; top: 10px; right: 10px; cursor: pointer;"
            ),
            choices = c(
              'Small/Medium',
              'Large',
              'California',
              'Washington and Oregon'
            ),
            selected = "Large"
          )
        ) # END div()
      ),

      bslib::layout_columns(
        # width = 1 / 5,
        # height = 175,
        fill = FALSE,
        ######################### Value Boxes #########################

        # Year value box
        bslib::value_box(
          title = "Year",
          value = textOutput(ns("year1_text")),
          theme = bslib::value_box_theme(
            bg = pal[["light_text"]],
            fg = pal[["bg_plot"]]
          ),
          showcase = bsicons::bs_icon("calendar")
        ),

        # Total Production Value value box
        bslib::value_box(
          title = "Total production value of all species",
          value = textOutput(ns("pval_text")),
          theme = bslib::value_box_theme(
            bg = pal[["value3"]],
            fg = pal[["bg_plot"]]
          ),
          showcase = bsicons::bs_icon("currency-dollar")
        ),

        # Change since year value box
        bslib::value_box(
          title = textOutput(ns("value_box_title")),
          value = uiOutput(ns("diff_text")),
          theme = bslib::value_box_theme(
            bg = pal[["dark_text"]],
            fg = pal[["bg_plot"]]
          ),
          showcase = bsicons::bs_icon("graph-up")
        ),

        # Number of observations value box
        bslib::value_box(
          title = "Number of processors",
          value = textOutput(ns("n_text")),
          theme = bslib::value_box_theme(
            bg = pal[["value4"]],
            fg = pal[["bg_plot"]]
          ),
          showcase = bsicons::bs_icon("hash")
        ),
      ),

      ######################### Plot Cards ##########################

      bslib::layout_column_wrap(
        width = 1 / 2,

        # production value lollipop graph
        bslib::card(
          full_screen = TRUE,
          class = "custom-card",
          bslib::card_header(
            "Species Production Value ($ Millions)",
            class = "bg-dark"
          ),

          bslib::card_body(
            plotOutput(ns("pv_plot")),
            class = "p-0" # remove padding
          )
        ),

        # production weight lollipop graph
        bslib::card(
          full_screen = TRUE,
          class = "custom-card",
          bslib::card_header(
            "Production Weight (lb Millions)",
            class = "bg-dark"
          ),
          bslib::card_body(
            plotOutput(ns("pw_plot")),
            class = "p-0" # remove padding
          )
        )
      )
    )
  )
}

######################################################################################
#################################### Server ##########################################
######################################################################################

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ############################# master data frame ################################

    # creating a reactive value to cache df
    master_df_cache <- reactiveVal()

    # creating a statement to re-render master df only when years change
    observe({
      req(input$year1Input, input$yearrangeInput)

      filtered <- overviewdf |>
        dplyr::filter(
          !is.na(.data[["value"]]),
          .data[["year"]] %in% # selecting just years that user is interested in
            c(
              input$year1Input, # from first year picker
              seq(input$yearrangeInput[1], input$yearrangeInput[2]) # from second year range slider
            ),
          # .data[["statistic"]] == "Total",
          # .data[["metric"]] %in% c("Production value", "Production weight"), # production value and weight for graphs
          .data[["type"]] %in% c(input$regsizeInput)
        ) |>
        dplyr::mutate(
          period = dplyr::case_when(
            .data[["year"]] == input$year1Input ~ "Selected Year", # labeling selected single year and range years for plotting
            TRUE ~ "Range Years"
          )
        )
      master_df_cache(filtered) # chaching master df in reactive value
    })

    # making master df a reactive of the cached results
    master_df <- reactive({
      req(master_df_cache())
      master_df_cache()
    })

    ############################# master data frame summary ################################

    # summary data frame for All production year 1 and range
    master_sum <- reactive({
      df <- master_df()

      df |>
        dplyr::filter(
          .data[["metric"]] == "Production value",
          .data[["variable"]] == "All production"
        ) |>
        dplyr::group_by(.data[["period"]]) |>
        dplyr::summarise(value = mean(.data[["value"]], na.rm = TRUE))
    })

    ############################# lollipop data frame ################################

    df_loli <- reactive({
      df <- master_df()

      # selecting data for just the range of dates
      range_data <- df |>
        dplyr::filter(
          .data[["year"]] %in%
            c(seq(input$yearrangeInput[1], input$yearrangeInput[2]))
        ) |>
        # summarizing means of the values across range
        dplyr::group_by(.data[["variable"]], .data[["metric"]]) |>
        dplyr::summarise(
          value = mean(.data[["value"]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          year = ifelse(
            # if it is a year range, bind the two years w/ "-" for the label
            input$yearrangeInput[1] != input$yearrangeInput[2],
            paste0(
              input$yearrangeInput[1],
              "-",
              input$yearrangeInput[2],
              " avg."
            ),
            # if year range is just one year, make the label just that year
            as.character(input$yearrangeInput[2])
          )
        ) # Label for legend

      # data for just the year
      year1_data <- df |>
        dplyr::filter(.data[["year"]] == input$year1Input) |>
        dplyr::mutate(year = as.character(.data[["year"]])) |>
        dplyr::select(
          "variable",
          "metric",
          "value",
          "year"
        )

      # binding data together for plotting
      plot_data <- dplyr::bind_rows(year1_data, range_data)

      return(plot_data)
    })

    ############################## Value Box Render #################################
    # Value Box Year 1 text render
    output$year1_text <- renderText({
      input$year1Input
    })

    # Value Box Production Value Text
    output$pval_text <- renderText({
      master_df() |>
        dplyr::filter(
          .data[["metric"]] == "Production value",
          .data[["variable"]] == "All production",
          .data[["year"]] == input$year1Input
        ) |>
        dplyr::pull(.data[["value"]]) |>
        round(2) |>
        paste("M")
    })

    # Value Box Difference title render
    output$value_box_title <- renderText({
      # text for date range
      if (input$yearrangeInput[1] != input$yearrangeInput[2]) {
        paste0(
          "Change since ",
          input$yearrangeInput[1],
          "-",
          input$yearrangeInput[2],
          " average"
        )
        # text for single year
      } else {
        paste0(
          "Change since ",
          input$yearrangeInput[1]
        )
      }
    })

    # Value box percent change value
    output$diff_text <- renderUI({
      # calculating % change

      df <- master_sum()

      # creating reactive values
      selected_val <- df$value[df$period == "Selected Year"]
      range_val <- df$value[df$period == "Range Years"]

      # calculate percent change if years are different
      diff <- if (length(selected_val) == 1 && length(range_val) == 1) {
        round((selected_val - range_val) / selected_val * 100, 1)
      } else {
        # make the value 0 if they are the same
        0
      }

      # assigning colors and +/- sign to value
      color <- if (diff > 0) "#76BC21" else "#DB2207"
      sign <- if (diff > 0) "+" else ""

      # HTML to change colors depending on value
      HTML(paste0("<span style='color:", color, "'>", sign, diff, "%</span>"))
    })

    # number of observations output
    output$n_text <- renderText({
      master_df() |>
        dplyr::filter(
          .data[["metric"]] == "Production value",
          .data[["variable"]] == "All production",
          .data[["year"]] == input$year1Input
        ) |>
        dplyr::pull(.data[["n"]])
    })

    ############################# Plots ##################################

    # production value chart
    output$pv_plot <- renderPlot({
      lollipop_func(
        data = dplyr::filter(
          df_loli(),
          .data[["metric"]] == "Production value"
        ),
        year1 = input$year1Input,
        range1 = input$yearrangeInput[1],
        range2 = input$yearrangeInput[2],
        upper_lim = 500
      )
    })

    # Production weight chart
    output$pw_plot <- renderPlot({
      lollipop_func(
        data = dplyr::filter(
          df_loli(),
          .data[["metric"]] == "Production weight"
        ),
        year1 = input$year1Input,
        range1 = input$yearrangeInput[1],
        range2 = input$yearrangeInput[2],
        upper_lim = 300
      )
    })
  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
