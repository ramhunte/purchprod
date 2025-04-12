#' ui
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

metric_func1 <- function(inputID) {
  checkboxGroupInput(
    inputId = inputID,
    label = "Metric",
    choices = c(
      "Markup",
      "Production price (per lb)",
      "Production value",
      "Production weight",
      "Purchase price (per lb)",
      "Recovery rate",
      "Purchase value",
      "Purchase weight"
    ),
    selected = c(
      "Markup",
      "Production price (per lb)",
      "Production value",
      "Production weight",
      "Purchase price (per lb)",
      "Recovery rate",
      "Purchase value",
      "Purchase weight"
    )
  )
}

metric_func2 <- function(inputID) {
  selectInput(
    inputId = inputID,
    label = "Select a metric",
    choices = c(
      'Production value',
      'Production price (per lb)',
      'Production weight'
    ),
    selectize = F
  )
}

# statostic UI function
stat_func <- function(inputID) {
  radioButtons(
    inputId = inputID,
    label = "Statistic",
    choices = c("Mean", "Median", "Total"),
    selected = "Median"
  )
}

# Product Type function
prodtype_func <- function(inputID) {
  checkboxGroupInput(
    inputId = inputID,
    label = "Product types",
    choices = c(
      "Canned",
      "Fillet",
      "Fresh",
      "Frozen",
      "Headed-and-gutted",
      "Other",
      "Unprocessed",
      "Smoked"
    ),
    selected = c(
      "Canned",
      "Fillet",
      "Fresh",
      "Frozen",
      "Headed-and-gutted",
      "Other",
      "Unprocessed",
      "Smoked"
    )
  )
}


specs_func <- function(inputID) {
  checkboxGroupInput(
    inputId = inputID,
    label = "Species",
    choices = c(
      "All production",
      "Groundfish production",
      "Pacific whiting",
      "Non-whiting groundfish",
      "Sablefish",
      "Rockfish",
      "Dover sole",
      "Petrale sole",
      "Thornyheads",
      "Other groundfish species"
    ),
    selected = "All production"
  )
}

os_func <- function(inputID1, inputID2) {
  shinyWidgets::dropdownButton(
    inputId = inputID1,
    label = "Other Species",
    circle = FALSE,
    checkboxGroupInput(
      inputId = inputID2,
      label = "",
      choices = c(
        "Other species production",
        "Crab",
        "Shrimp",
        "Salmon",
        "Tuna",
        "Coastal pelagics",
        "Other shellfish",
        "Other species"
      )
    )
  )
}

reg_func <- function(inputID) {
  checkboxGroupInput(
    inputId = inputID,
    label = "",
    choices = c(
      'California',
      'Washington and Oregon'
    ),
    selected = "California"
  )
}

# production activities function
pracs_func <- function(inputID) {
  radioButtons(
    inputId = inputID,
    label = "Production activities",
    choices = c(
      'All production',
      'Groundfish production',
      'Other species production'
    ),
    selected = "All production"
  )
}

# processor size function
size_func <- function(inputID) {
  checkboxGroupInput(
    inputId = inputID,
    label = "",
    choices = c(
      'Small',
      'Medium',
      'Large',
      'Non-processor'
    ),
    selected = "Small"
  )
}

down_func <- function(outputID) {
  downloadButton(
    outputId = outputID,
    label = "Download",
    class = "btn-secondary custom-download"
  )
}

# conditional panel functions

species_tabs_func <- function() {
  bslib::navset_card_pill(
    bslib::nav_panel(
      "Product Type",
      class = "custom-card",
      prodtype_func("protype2Input")
    ),
    id = "tab_bottom"
  )
}

other_tabs_func <- function() {
  bslib::navset_card_pill(
    bslib::nav_panel(
      "Production Activities",
      class = "custom-card",
      specs_func(inputID = "prodacInput"),
      os_func(inputID1 = "osDropdown", inputID2 = "ospsInput")
    ),
    bslib::nav_panel(
      "Region",
      class = "custom-card",
      reg_func(inputID = "regionInput"),
      pracs_func(inputID = "pracs1Input")
    ),
    bslib::nav_panel(
      "Processor Size/Type",
      class = "custom-card",
      size_func(inputID = "sizeInput"),
      pracs_func(inputID = "pracs2Input")
    ),
    id = "tab_bottom"
  )
}
