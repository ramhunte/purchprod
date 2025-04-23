#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

######################################################################################
####################################### UI ###########################################
######################################################################################

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_fillable(
      bslib::layout_columns(
        fill = FALSE,
        ######################### Year Picker Card #########################
        bslib::card(
          class = "card-overflow-fix",
          year_func(
            inputID = ns("year1Input"),
            label = "Select a year:",
            choices = unique(clean_purcprod$year),
            selected = "2023",
            options = list(`style` = "btn-year1")
          ),

          # year picker value box
          year_range_func(
            inputID = ns("yearrangeInput"),
            label = "Select a date range:",
            min = min(clean_purcprod$year),
            max = max(clean_purcprod$year),
            value = c(2015, 2020)
            # options = list(`style` = "btn-year2")
          )
        ),

        ######################### Value Boxes #########################

        # year display value box
        bslib::value_box(
          title = "Year",
          value = textOutput(ns("year1_text")),
          theme = bslib::value_box_theme(
            bg = pal[["light_text"]],
            fg = "#E9F3F6"
          ),
          showcase = bsicons::bs_icon("calendar")
        ),

        # Production value box
        bslib::value_box(
          title = "Total Production Value",
          value = textOutput(ns("pval_text")),
          theme = bslib::value_box_theme(bg = "#5EB6D9", fg = "#E9F3F6"),
          showcase = bsicons::bs_icon("currency-dollar")
        ),

        # Percent change value box
        bslib::value_box(
          # title = uiOutput(ns("value_box_title")),
          title = textOutput(ns("value_box_title")),
          value = uiOutput(ns("diff_text")),
          theme = bslib::value_box_theme(
            bg = pal[["dark_text"]],
            fg = "#E9F3F6"
          ),
          showcase = bsicons::bs_icon("graph-up")
        ),

        # Number of observations value box
        bslib::value_box(
          title = "Number of observations",
          value = textOutput(ns("n_text")),
          theme = bslib::value_box_theme(bg = "#90DFE3", fg = "#E9F3F6"),
          showcase = bsicons::bs_icon("hash")
        ),
      ),

      ######################### Plot Cards ##########################

      bslib::layout_column_wrap(
        width = 1 / 2,

        # lollipop chart
        bslib::card(
          full_screen = TRUE,
          class = "custom-card p-0",
          bslib::card_header(
            "Species Production Value ($ Millions)",
            class = "bg-dark"
          ),

          bslib::card_body(
            plotOutput(ns("lolli_plot")),
            class = "p-0" # remove padding
          )
        ),

        # bar chart
        bslib::card(
          full_screen = TRUE,
          class = "custom-card",
          bslib::card_header(
            "Product Type Production Value ($ Millions)",
            class = "bg-dark"
          ),
          bslib::card_body(
            plotOutput(ns("bar_plot")),
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

    # master reactive data frame for values and plotting
    master_df <- reactive({
      sumdf_prac |>
        dplyr::filter(
          year %in%
            c(
              input$year1Input,
              seq(input$yearrangeInput[1], input$yearrangeInput[2])
            ),
          statistic == "Total",
          metric == "Production value"
        ) |>
        dplyr::mutate(
          period = dplyr::case_when(
            year == 2017 ~ "Selected Year",
            TRUE ~ "Range Years"
          )
        )
    })

    # data frame for All production
    df <- reactive({
      master_df() |>
        dplyr::filter(variable == "All production")
    })

    # summary data frame for All production year 1 and range
    df_sum <- reactive({
      df() |>
        dplyr::mutate(
          period = dplyr::case_when(
            year == input$year1Input ~ "Selected Year",
            TRUE ~ "Range Years"
          )
        ) |>
        dplyr::group_by(period) |>
        dplyr::summarise(value = mean(value, na.rm = TRUE))
    })

    ############################# lollipop data frame ################################

    df_loli <- reactive({
      # selecting data for just the range of dates
      range_data <- master_df() |>
        dplyr::filter(
          year %in% c(seq(input$yearrangeInput[1], input$yearrangeInput[2]))
        ) |>
        # summarizing means of the values across range
        dplyr::group_by(variable) |>
        dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(
          year = paste0(input$yearrangeInput[1], "–", input$yearrangeInput[2])
        ) # Label for legend

      # data for just the year
      year1_data <- master_df() |>
        dplyr::filter(year == input$year1Input) |>
        dplyr::mutate(year = as.character(year)) |>
        dplyr::select(variable, value, year)

      # binding data together for plotting
      plot_data <- dplyr::bind_rows(year1_data, range_data)

      return(plot_data)
    })
    ######################################################################################
    # df_bar <- reactive({
    #   proddf_prac |>
    #     dplyr::filter(
    #       year %in% c(input$year1Input, input$year2Input),
    #       statistic == "Total",
    #       metric == "Production value",
    #       variable == "All production"
    #     )
    # })

    ############################## Value Box Render #################################
    # Value Box Year 1 text render
    output$year1_text <- renderText({
      input$year1Input
    })

    # Value Box Production Value Text
    output$pval_text <- renderText({
      df() |>
        dplyr::filter(year == input$year1Input) |>
        dplyr::pull(value) |>
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
      diff <- round(
        (df_sum()$value[df_sum()$period == "Selected Year"] -
          df_sum()$value[df_sum()$period == "Range Years"]) /
          df_sum()$value[df_sum()$period == "Selected Year"] *
          100,
        1
      )

      # assigning colros and +/- sign to value
      color <- if (diff > 0) "#4B8320" else "#DB2207"
      sign <- if (diff > 0) "+" else ""

      # HTML to change colors depending on value
      HTML(paste0("<span style='color:", color, "'>", sign, diff, "%</span>"))
    })

    # number of observations output
    output$n_text <- renderText({
      df()$n[df()$year == input$year1Input]
    })

    ############################# Plots ##################################

    # lollipot chart
    output$lolli_plot <- renderPlot({
      lollipop_func(
        data = df_loli(),
        year1 = input$year1Input,
        range1 = input$yearrangeInput[1],
        range2 = input$yearrangeInput[2]
      )
    })

    # bar chart
    output$bar_plot <- renderPlot({
      barplot_func(
        data = df_bar(),
        year1 = input$year1,
        year2 = input$year2
      )
    })
  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
