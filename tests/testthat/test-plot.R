############################ Testing the lollipop chart ##############################
test_that("lollipop_func generates correct ggplot object", {
  # Create a simple example dataset
  data <- data.frame(
    value = c(10, 15, 20, 25),
    variable = c("A", "B", "C", "D"),
    year = c(2020, 2020, 2021, 2021)
  )

  # Call the function with this dataset
  plot <- lollipop_func(
    data,
    year1 = 2020,
    range1 = 2020,
    range2 = 2021,
    upper_lim = 30
  )

  # Check if the result is a ggplot object
  expect_s3_class(plot, "gg")

  # Optionally, check some properties of the plot
  expect_true("ggplot" %in% class(plot))

  # Test edge case with a single year
  plot_single_year <- lollipop_func(
    data,
    year1 = 2020,
    range1 = 2020,
    range2 = 2020,
    upper_lim = 30
  )
  expect_true("ggplot" %in% class(plot_single_year))
})

############################ Testing the linegraph chart ##############################

test_that("plot_func generates correct ggplot object", {
  # Create a simple example dataset
  data <- data.frame(
    year = rep(2020:2022, each = 3),
    value = c(10, 20, 30, 15, 25, 35, 10, 30, 40),
    group = rep(c("A", "B", "C"), times = 3),
    lower = c(8, 18, 28, 12, 22, 32, 8, 28, 38),
    upper = c(12, 22, 32, 18, 28, 38, 12, 32, 42)
  )

  # Call the function with this dataset
  plot <- plot_func(
    data,
    lab = "Value",
    group = "group",
    facet = "group",
    line = "solid",
    title = "Test Plot"
  )

  # Check if the result is a ggplot object
  expect_s3_class(plot, "gg")
})
