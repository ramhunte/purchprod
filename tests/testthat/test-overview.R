######################################################################
############################### Server ###############################
######################################################################

test_that("mod_overview_ui renders correctly", {
  ui <- mod_overview_ui("test")

  expect_type(ui, "list") # Should return a tagList
  expect_true(any(grepl("year1Input", as.character(ui)))) # Year picker exists
  expect_true(any(grepl("yearrangeInput", as.character(ui)))) # Year range slider exists
  expect_true(any(grepl("pv_plot", as.character(ui)))) # PV plot exists
  expect_true(any(grepl("pw_plot", as.character(ui)))) # PW plot exists
  expect_true(any(grepl("value_box_title", as.character(ui)))) # Value box title exists
})


######################################################################
############################### Server ###############################
######################################################################
test_that("master_df reacts to year inputs", {
  testServer(mod_overview_server, {
    session$setInputs(year1Input = 2020, yearrangeInput = c(2015, 2020))

    master <- master_df()

    expect_s3_class(master, "data.frame")
    expect_true(all(master$year %in% c(2020, 2015:2020)))
    expect_true(all(
      master$metric %in% c("Production value", "Production weight")
    ))
  })
})

test_that("master_sum returns correct structure", {
  testServer(mod_overview_server, {
    session$setInputs(year1Input = 2020, yearrangeInput = c(2015, 2020))

    sum_df <- master_sum()
    expect_s3_class(sum_df, "data.frame")
    expect_true(all(c("period", "value") %in% names(sum_df)))
  })
})

test_that("value box renders correct title", {
  testServer(mod_overview_server, {
    session$setInputs(year1Input = 2020, yearrangeInput = c(2015, 2020))
    expect_match(output$value_box_title, "Change since 2015-2020 average")
  })
})


test_that("plots return without error", {
  testServer(mod_overview_server, {
    session$setInputs(year1Input = 2020, yearrangeInput = c(2015, 2020))
    expect_silent(output$pv_plot)
    expect_silent(output$pw_plot)
  })
})
