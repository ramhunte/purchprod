################################ Overview page ################################

###################### year picker (single year)
test_that("year_func returns correct pickerInput", {
  ui <- year_func(
    inputID = "yearInput",
    label = "Select a Year",
    choices = 2010:2020,
    selected = 2015,
    options = list(style = "btn-primary")
  )

  # Check tag class
  expect_s3_class(ui, "shiny.tag")

  # Check inputId via convert to HTML string
  html <- as.character(ui)

  # Confirm it contains expected ID and label
  expect_match(html, 'id="yearInput"')
  expect_match(html, '>Select a Year<')
})

#######################  Year slider
test_that("year_range_func returns correct sliderInput", {
  ui <- year_range_func(
    inputID = "yearRange",
    label = "Select Year Range",
    min = 2000,
    max = 2025,
    value = c(2010, 2020)
  )

  # Check that it's a shiny.tag object
  expect_s3_class(ui, "shiny.tag")

  # Convert to HTML and check that attributes are correctly included
  html <- as.character(ui)

  # Check for correct inputId and label
  expect_match(html, 'id="yearRange"')
  expect_match(html, '>Select Year Range<')

  # Check for the presence of min and max values
  expect_match(html, 'min="2000"')
  expect_match(html, 'max="2025"')
})

################################ Explore the Data page ################################

#######################  Metric 1
test_that("metric_func1 returns checkboxGroupInput with correct properties", {
  ui <- metric_func1("metricInput")

  # Basic type check
  expect_s3_class(ui, "shiny.tag")

  # Render HTML
  html <- as.character(ui)

  # Check for correct inputId and label
  expect_match(html, 'id="metricInput"')
  expect_match(html, '>Metric<')

  # Check that some of the expected choices are present
  expect_match(html, 'Markup')
  expect_match(html, 'Production price \\(per lb\\)')
  expect_match(html, 'Purchase value')
})


#######################  Metric 2
test_that("metric_func2 returns selectInput with correct properties", {
  ui <- metric_func2("metricInput")

  # Basic type check
  expect_s3_class(ui, "shiny.tag")

  # Render HTML
  html <- as.character(ui)

  # Check for correct inputId and label
  expect_match(html, 'id="metricInput"')
  expect_match(html, '>Select a metric<')

  # Check that the correct choices are present
  expect_match(html, 'Production value')
  expect_match(html, 'Production price \\(per lb\\)')
  expect_match(html, 'Production weight')
})

####################### Statistic
# test_that("stat_func returns radioButtons with correct properties", {
#   ui <- stat_func("statInput")
#
#   # Basic type check
#   expect_s3_class(ui, "shiny.tag")
#
#   # Render HTML
#   html <- as.character(ui)
#
#   # Check for correct inputId and label
#   expect_match(html, 'id="statInput"')
#   expect_match(html, '>Statistic<')
#
#   # Check that the correct choices are present
#   expect_match(html, 'Mean')
#   expect_match(html, 'Median')
#   expect_match(html, 'Total')
#
#   # Check that the default selected option is "Median"
#   expect_match(html, 'checked="checked"') # Look for the "Median" being checked
# })

####################### Product type
test_that("prodtype_func returns checkboxGroupInput with correct properties", {
  ui <- prodtype_func("prodtypeInput")

  # Basic type check
  expect_s3_class(ui, "shiny.tag")

  # Render HTML
  html <- as.character(ui)

  # Check for correct inputId and label
  expect_match(html, 'id="prodtypeInput"')
  expect_match(html, '>Product types<')

  # Check that some of the expected choices are present
  expect_match(html, 'Canned')
  expect_match(html, 'Fillet')
  expect_match(html, 'Fresh')

  # Check that all the default selected options are correctly set
  expect_match(html, 'checked="checked"') # For example, checking if "Canned" is selected
})

####################### Species type
test_that("specs_func returns checkboxGroupInput with correct properties", {
  ui <- specs_func("speciesInput")

  # Basic type check
  expect_s3_class(ui, "shiny.tag")

  # Render HTML
  html <- as.character(ui)

  # Check for correct inputId and label
  expect_match(html, 'id="speciesInput"')
  expect_match(html, '>Species<')

  # Check that some of the expected choices are present
  expect_match(html, 'All production')
  expect_match(html, 'Rockfish')
  expect_match(html, 'Dover sole')

  # Check that the default selected options are correctly set
  expect_match(html, 'checked="checked"') # For example, checking if "All production" is selected
})

####################### Other Species
test_that("os_func returns dropdownButton with correct properties", {
  ui <- os_func("osDropdown", "osCheckboxInput")

  # Basic type check
  expect_s3_class(ui, "shiny.tag")

  # Render HTML
  html <- as.character(ui)

  # Check for correct inputId and label
  expect_match(html, 'id="osDropdown"')
  expect_match(html, '><')

  # Check for checkbox input within dropdown
  expect_match(html, 'id="osCheckboxInput"')
  expect_match(html, 'Crab')
  expect_match(html, 'Tuna')
})

####################### Region
test_that("reg_func returns checkboxGroupInput with correct properties", {
  ui <- reg_func("regionInput")

  # Basic type check
  expect_s3_class(ui, "shiny.tag")

  # Render HTML
  html <- as.character(ui)

  # Check for correct inputId and label
  expect_match(html, 'id="regionInput"')

  # Check that the expected region choices are present
  expect_match(html, 'California')
  expect_match(html, 'Washington and Oregon')
})

####################### Production activities
test_that("pracs_func returns radioButtons with correct properties", {
  ui <- pracs_func("pracsInput")

  # Basic type check
  expect_s3_class(ui, "shiny.tag")

  # Render HTML
  html <- as.character(ui)

  # Check for correct inputId and label
  expect_match(html, 'id="pracsInput"')
  expect_match(html, '>Production activities<')

  # Check that the correct choices are present
  expect_match(html, 'All production')
  expect_match(html, 'Groundfish production')
  expect_match(html, 'Other species production')
})

####################### Size
test_that("size_func returns checkboxGroupInput with correct properties", {
  ui <- size_func("sizeInput")

  # Basic type check
  expect_s3_class(ui, "shiny.tag")

  # Render HTML
  html <- as.character(ui)

  # Check for correct inputId and label
  expect_match(html, 'id="sizeInput"')

  # Check that the expected processor size choices are present
  expect_match(html, 'Small')
  expect_match(html, 'Medium')
  expect_match(html, 'Large')
  expect_match(html, 'Non-processor')
})

####################### Download Button

test_that("down_func returns downloadButton with correct properties", {
  ui <- down_func("downloadBtn")

  # Basic type check
  expect_s3_class(ui, "shiny.tag")

  # Render HTML
  html <- as.character(ui)

  # Check for correct outputId and label
  expect_match(html, 'id="downloadBtn"')
})

####################### Conditional Panel render: Other tabs ("Summary" and "By Product Type")
test_that("other_tabs_func returns navset_card_pill with correct panels", {
  ui <- other_tabs_func()

  # Basic type check
  expect_s3_class(ui, "shiny.tag")

  # Render HTML
  html <- as.character(ui)

  # Check for the main navset card pill container ID
  expect_match(html, 'id="tab_bottom"')

  # Check for the nav panels and their labels
  expect_match(html, 'Production Activities')
  expect_match(html, 'Region')
  expect_match(html, 'Processor Size/Type')

  # Ensure the panels contain the expected input IDs and elements
  expect_match(html, 'id="prodacInput"')
  expect_match(html, 'id="osDropdown"')
  expect_match(html, 'id="regionInput"')
  expect_match(html, 'id="pracs1Input"')
  expect_match(html, 'id="sizeInput"')
  expect_match(html, 'id="pracs2Input"')
})

####################### Conditional Panel render: Species tabs ("By Species")

test_that("species_tabs_func returns navset_card_pill with correct panel", {
  ui <- species_tabs_func()

  # Basic type check
  expect_s3_class(ui, "shiny.tag")

  # Render HTML
  html <- as.character(ui)

  # Check for the main navset card pill container ID
  expect_match(html, 'id="tab_bottom"')

  # Check for the nav panel and its label
  expect_match(html, 'Product Type')

  # Ensure the panel contains the expected input ID
  expect_match(html, 'id="protype2Input"')
})
