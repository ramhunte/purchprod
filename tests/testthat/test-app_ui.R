test_that("app_ui renders correctly and includes key UI elements", {
  ui <- app_ui(request = NULL)

  # Check that it returns a shiny.tag.list object (tagList returns this)
  expect_s3_class(ui, "shiny.tag.list")

  # Check for presence of navbar page
  expect_true(any(grepl("page-navbar", as.character(ui))))

  # Check for expected tab titles
  ui_str <- as.character(ui)
  expect_true(any(grepl("Overview", ui_str)))
  expect_true(any(grepl("Explore the Data", ui_str)))
  expect_true(any(grepl("Information Page", ui_str)))
  expect_true(any(grepl("Contact Us", ui_str)))

  # Check that module UIs are present
  expect_true(any(grepl("overview_1", ui_str)))
  expect_true(any(grepl("summary_1", ui_str)))
  expect_true(any(grepl("prod_type_1", ui_str)))
  expect_true(any(grepl("specs_1", ui_str)))

  # Check that the plot and table outputs exist
  expect_true(any(grepl("exp_plot_ui", ui_str)))
  expect_true(any(grepl("table", ui_str)))

  # Check that footer and header are included
  expect_true(any(grepl("footer", ui_str)))
  expect_true(any(grepl("header", ui_str)))
})
