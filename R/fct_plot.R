#' plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#'@import ggplot2
#'
#' @noRd

# setting aesthetics of plot

# line colors ----
line_col <- c(
  # species
  "All production" = "black",
  "Groundfish production" = '#C1052F',
  "Pacific whiting" = '#D89B2C',
  "Non-whiting groundfish" = '#C0CB81',
  "Sablefish" = '#648C1C',
  "Rockfish" = '#6FB1C9',
  "Dover sole" = '#001B70',
  "Petrale sole" = '#595478',
  "Thornyheads" = '#C0B3B6',
  "Other groundfish species" = '#B56C97',
  "Other species production" = '#C1052F',
  "Crab" = '#D89B2C',
  "Shrimp" = '#C0CB81',
  "Salmon" = '#648C1C',
  "Tuna" = '#6FB1C9',
  "Coastal pelagics" = '#001B70',
  "Other shellfish" = '#595478',
  "Other species" = '#C0B3B6',

  # states
  "California" = '#001B70',
  "Washington and Oregon" = '#C1052F',

  # processor size
  "Small" = '#001B70',
  "Medium" = '#C1052F',
  "Large" = '#648C1C',
  "Non-processor" = '#D89B2C',

  # product type
  "Canned" = "#287271",
  "Fillet" = "#9E2B25",
  "Fresh" = "#208AAE",
  "Frozen" = "#FF9F1C",
  "Headed-and-gutted" = "#8E6C8A",
  "Other" = "#B1B695",
  "Unprocessed" = "#607744",
  "Smoked" = "#D77A61"
)

# line type ----
line_ty <- c(
  # states
  "California" = 'solid',
  "Washington and Oregon" = 'solid',

  # processor size
  "Small" = 'solid',
  "Medium" = 'solid',
  "Large" = 'solid',
  "Non-processor" = 'solid',

  # species
  "All production" = "solid",
  "Groundfish production" = 'solid',
  "Pacific whiting" = 'solid',
  "Non-whiting groundfish" = 'solid',
  "Sablefish" = 'solid',
  "Rockfish" = 'solid',
  "Dover sole" = 'solid',
  "Petrale sole" = 'solid',
  "Thornyheads" = 'solid',
  "Other groundfish species" = 'solid',

  # other species
  "Other species production" = 'dashed',
  "Crab" = 'dashed',
  "Shrimp" = 'dashed',
  "Salmon" = 'dashed',
  "Tuna" = 'dashed',
  "Coastal pelagics" = 'dashed',
  "Other shellfish" = 'dashed',
  "Other species" = 'dashed',

  # product type
  "Canned" = "solid",
  "Fillet" = "solid",
  "Fresh" = "solid",
  "Frozen" = "solid",
  "Headed-and-gutted" = "solid",
  "Other" = "solid",
  "Unprocessed" = "solid",
  "Smoked" = "solid"
)

plot_func <- function(data, lab, group, facet) {
  # return nothing if plot is Null
  validate(
    need(data, "No data available for these selected inputs"),
    need(
      nrow(data) > 0,
      "No data available for these selected inputs"
    )
  )

  # ggplot code
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = year, y = value, group = .data[[group]])
  ) +
    scale_fill_manual(values = line_col) +
    scale_color_manual(values = line_col) +
    scale_linetype_manual(values = line_ty) +
    theme_minimal() +
    labs(
      y = lab,
      x = "Year"
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(
      text = element_text(size = 22),
      axis.text = element_text(size = 18),
      strip.text = element_text(size = 18),
      legend.title = element_blank(),
      legend.position = "bottom", # Moves the legend to the bottom
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(size = 1.2),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(size = 1.2),
      axis.line = element_line(color = "grey", linewidth = 1) # Adds borders to only x and y axes
    ) +
    geom_point(aes(color = .data[[group]]), size = 4) +
    geom_line(
      aes(color = .data[[group]], linetype = .data[[group]]),
      linewidth = 0.75
    ) +
    geom_ribbon(
      aes(ymax = upper, ymin = lower, fill = .data[[group]]),
      alpha = .2
    ) +
    # facet wrap based on the column specified to be faceted in the function
    facet_wrap(as.formula(paste("~", facet)), scales = 'free_y', ncol = 2)
}

# function for processing DT render data
process_df <- function(df) {
  df |>
    dplyr::select(-c(ylab, tab, unit_lab)) |>
    dplyr::mutate(
      variance = round(variance, 2),
      q25 = round(q25, 2),
      q75 = round(q75, 2),
      value = round(value, 2),
      lower = round(lower, 2),
      upper = round(upper, 2)
    )
}
