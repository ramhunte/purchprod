#' ui
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# This script contains all of the functions I used for UI widgets and conditional panel display
# these are called throughout the mod_*.R files and the app_*.R files
# some are repeated in modules and some are used only once

################################ Overview page ################################

# creates a select year picker (single year)
year_func <- function(inputID, label, choices, selected, options = NULL) {
  shinyWidgets::pickerInput(
    inputId = inputID,
    label = label,
    choices = choices,
    selected = selected,
    options = options
  )
}

# creates a select year slider (multiple years/range)
year_range_func <- function(inputID, label, min, max, value) {
  sliderInput(
    inputId = inputID,
    label = label,
    min = min,
    max = max,
    value = value,
    sep = ""
  )
}

################################ Explore the Data page ################################

# creates a checkbox option for metric to choose from
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


# creates a select dropdown option for metric to choose from (less options than metric_func1)
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

# creates a radiobutton picker to choose statistic
stat_func <- function(inputID) {
  radioButtons(
    inputId = inputID,
    label = "Statistic",
    choices = c("Mean", "Median", "Total"),
    selected = "Median"
  )
}

# creates a checkbox to choose product type
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

# creates a checkbox to choose species
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
    selected = c(
      "All production",
      "Groundfish production",
      "Pacific whiting",
      "Rockfish",
      "Dover sole",
      "Petrale sole"
    )
  )
}

# creates a dropdown to choose other species (not main species of interest)
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

# creates a checkbox to choose region
reg_func <- function(inputID) {
  checkboxGroupInput(
    inputId = inputID,
    label = "",
    choices = c(
      'California',
      'Washington and Oregon'
    ),
    selected = c(
      'California',
      'Washington and Oregon'
    ),
  )
}

# creates a radio buttons list to choose production activities (species) (less options than specs_func)
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

# creates a checkbox to choose processor size
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
    selected = c(
      'Small',
      'Medium',
      'Large'
    )
  )
}


# Deflator picker
defl_func <- function(
  inputID,
  label,
  choices,
  selected,
  options = NULL,
  width = NULL
) {
  shinyWidgets::pickerInput(
    inputId = inputID,
    label = label,
    choices = choices,
    selected = selected,
    options = options,
    width = width
  )
}


# creates a download button for csv
down_func <- function(outputID) {
  downloadButton(
    outputId = outputID,
    label = "Download",
    class = "btn-secondary custom-download"
  )
}


# conditional nav_panel render depending on which tab_top is selected
# essentially these functions display nav_panels() in the bottom left sidebar of the page. The data
# to be show is different depending on which tab_top you are looking at ("Summary" or "By Product Type" vs "Species")

# this renders a navset_card_pill() + panels when tab_top "Summary" or "By Product Type" is selected
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

# this renders a navset_card_pill() + panels when tab_top "By Species" is selected
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
