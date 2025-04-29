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
