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
    # Your application UI logic
    # page_fluid(
    bslib::page_navbar(
      # calling themes for the page elements
      theme = bslib::bs_theme(
        bootswatch = "materia",
        secondary = "#5EB6D9",
        fg = "#001743",
        bg = "#C2D9E3",
        # "#00797F",
        primary = "#00559B",

        # "#00559B",
        'navbar-bg' = "#001743"
      ),

      # START page_navbar
      # page_navbar(
      id = "navbar",
      title = "Purchase Production App",

      header(),

      bslib::nav_panel(
        "Explore the Data",

        # START sidebarLayout
        bslib::page_sidebar(
          # style = "min-height: 80vh;",
          ########################### Top tabSet ######################################
          #################### (Summary; By Product Type) #############################

          # START sidbarPanel
          sidebar = bslib::sidebar(
            width = 500,
            # START tabsetPanel
            bslib::navset_card_pill(
              bslib::nav_panel(
                "Summary",
                class = "custom-card",
                mod_summary_ui("summary_1")
              ), # END Summary nav_panel
              # START "Product Type" tabPanel
              bslib::nav_panel(
                "By Product Type",
                class = "custom-card",
                # Metric
                mod_prod_type_ui("prod_type_1")
              ), #END Product Type nav_panel
              bslib::nav_panel(
                "Species",
                class = "custom-card",
                mod_specs_ui("specs_1")
              ), # END Species nav_panel
              id = "tab_top"
            ), #END navset_card_pill
            ############################### Bottom tabSet ###################################
            ############# (Production Activities; Region; Processor size/type) ##############

            # START tabsetPanel

            # uiOutput("dynamicTabs"),

            # uiOutput("bottomTabs"),

            conditionalPanel(
              condition = "input.tab_top != 'Species'",
              # mod_other_tabs_ui("other_tabs_1")
              uiOutput("otherTabs")
            ),

            conditionalPanel(
              condition = "input.tab_top == 'Species'",
              # mod_specs_tabs_ui("specs_tabs_1")
              uiOutput("speciesTabs")
            ),

            # downloadButton
            down_func(outputID = "downloadData")
          ), # END sidebar,

          ########################### mainPanel #######################################

          # START mainPanel
          bslib::navset_card_pill(
            # START "Plot" nav_panel
            bslib::nav_panel(
              title = "Plot",
              class = "custom-card",
              conditionalPanel(
                condition = "input.tab_top == 'Summary'",
                shinycssloaders::withSpinner(
                  # adding a cool loader
                  plotOutput("sumplot", width = "100%", height = "575px")
                )
              ),
              conditionalPanel(
                condition = "input.tab_top == 'By Product Type'",
                shinycssloaders::withSpinner(
                  # adding a cool loader
                  plotOutput("productplot", width = "100%", height = "575px")
                )
              ),

              conditionalPanel(
                condition = "input.tab_top == 'Species'",
                shinycssloaders::withSpinner(
                  # adding a cool loader
                  plotOutput("specsplot", width = "100%", height = "575px")
                )
              )
            ), # END Plot nav_panel

            # START "Table" nav_panel
            bslib::nav_panel(
              "Table",
              class = "custom-card",
              shinycssloaders::withSpinner(
                # adding a cool loader
                dataTableOutput("table")
              )
            ) # END  "Table" nav_panel
          ) # END main panel
        ) # END page_sidebar
      ), # END "Explore the Data" nav_panel

      # START "Information Page" nav_panel
      bslib::nav_panel(
        "Information Page",
        # style = page_height,
        fluidRow(
          # use columns to create white space on sides
          # column(1),
          # column(10, includeMarkdown("text/info.md")),
          # column(1)
        ),
      ), # END "Information Page" nav_panel

      # START "Bulletin Board" nav_panel
      bslib::nav_panel(
        "Bulletin Board",
        # style = page_height,
      ), # END "Bulletin Board" nav_panel

      # START "Contact" Us nav_panel
      bslib::nav_panel(
        "Contact Us",
        # style = page_height,
        fluidRow(
          # use columns to create white space on sides
          # column(1),
          # column(10, includeMarkdown("text/contact.md")),
          # column(1)
        )
      ), # "Contact" Us nav_panel

      footer = footer() # END footer
    ) # END page_navbar
  ) # END tagList
}
#
#
#
#
#
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
