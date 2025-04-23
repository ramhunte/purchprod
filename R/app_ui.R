#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    bslib::page_navbar(
      # calling themes for the page elements
      theme = bslib::bs_theme(
        bootswatch = "materia",
        secondary = "#5EB6D9",
        fg = "#001743",
        bg = "#C2D9E3",
        # "#00797F",
        primary = "#00559B",
        'navbar-bg' = "#001743"
      ),

      # START page_navbar
      id = "navbar",
      title = "Purchase Production App",

      # NOAA Fisheries logo
      header(),

      # Overview tab
      bslib::nav_panel(
        "Overview",
        mod_overview_ui("overview_1")
      ),

      # Explore the Data tab
      bslib::nav_panel(
        "Explore the Data",

        # START page_sidebar
        bslib::page_sidebar(
          ########################### Top tabSet ######################################
          #################### (Summary; By Product Type) #############################

          # START sidbarPanel
          sidebar = bslib::sidebar(
            width = 500,
            # START "Summary" nav_panel
            bslib::navset_card_pill(
              bslib::nav_panel(
                "Summary",
                class = "custom-card",
                mod_summary_ui("summary_1")
              ), # END Summary nav_panel
              # START By Product Type nav_panel
              bslib::nav_panel(
                "By Product Type",
                class = "custom-card",
                # Metric
                mod_prod_type_ui("prod_type_1")
              ), # END By Product Type nav_panel

              # START By Species nav_panel
              bslib::nav_panel(
                "By Species",
                class = "custom-card",
                mod_specs_ui("specs_1")
              ), # END By Species nav_panel
              id = "tab_top"
            ), #END navset_card_pill
            ############################### Bottom tabSet ###################################
            ############# (Production Activities; Region; Processor size/type) ##############

            # START lower nav_panel

            conditionalPanel(
              condition = "input.tab_top != 'By Species'",
              # mod_other_tabs_ui("other_tabs_1")
              uiOutput("otherTabs")
            ),

            conditionalPanel(
              condition = "input.tab_top == 'By Species'",
              # mod_specs_tabs_ui("specs_tabs_1")
              uiOutput("speciesTabs")
            ),

            # END lower nav_panel

            # downloadButton
            down_func(outputID = "downloadData")
          ), # END sidebar,

          ########################### mainPanel #######################################

          # START main panel navset_card_pill
          bslib::navset_card_pill(
            # START Plot nav_panel main
            bslib::nav_panel(
              title = "Plot",
              class = "custom-card",

              # condition for summary tab
              conditionalPanel(
                condition = "input.tab_top == 'Summary'",
                shinycssloaders::withSpinner(
                  # adding a cool loader
                  plotOutput("sumplot", width = "100%", height = "620px")
                )
              ),

              #condition for By product Type tab
              conditionalPanel(
                condition = "input.tab_top == 'By Product Type'",
                shinycssloaders::withSpinner(
                  # adding a cool loader
                  plotOutput("productplot", width = "100%", height = "620px")
                )
              ),

              # condition for By Species tab
              conditionalPanel(
                condition = "input.tab_top == 'By Species'",
                shinycssloaders::withSpinner(
                  # adding a cool loader
                  plotOutput("specsplot", width = "100%", height = "620px")
                )
              )
            ), # END Plot nav_panel

            # START Table nav_panel
            bslib::nav_panel(
              "Table",
              class = "custom-card",
              shinycssloaders::withSpinner(
                # adding a cool loader
                dataTableOutput("table")
              )
            ) # END  Table nav_panel
          ) # END main panel navset_card_pill
        ) # END page_sidebar
      ), # END Explore the Data nav_panel

      # START Information Page nav_panel
      bslib::nav_panel(
        "Information Page",
        # style = page_height,
        fluidRow(
          # use columns to create white space on sides
          column(2),
          column(8, includeMarkdown("inst/app/text/info.md")),
          column(2)
        ),
      ), # END "Information Page" nav_panel

      # START Contact Us nav_panel
      bslib::nav_panel(
        "Contact Us",
        fluidRow(
          # use columns to create white space on sides
          column(2),
          column(8, includeMarkdown("inst/app/text/contact.md")),
          column(2)
        )
      ), # Contact Us nav_panel

      footer = footer() # END footer
    ) # END page_navbar
  ) # END tagList
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "purchprod"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
