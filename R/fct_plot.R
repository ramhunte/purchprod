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

pal <- c(
  light_text = "#0085CA",
  dark_text = "#003087",
  value1 = "#005E5E",
  value2 = "#C2D9E3",
  bg_plot = "#E9F3F6"
)


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

############################## Overview plots ##################################

############################## Barplot  ##################################

# remotes::install_github("hrbrmstr/ggchicklet")

barplot_func <- function(data, year1, year2) {
  # making a barchart
  barchart <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = forcats::fct_reorder(type, value),
      y = value,
      fill = factor(year)
    )
  ) +
    # adding count text to each bar
    ggchicklet::geom_chicklet(
      position = "dodge",
      radius = grid::unit(5, "pt")
    ) +
    ggplot2::labs(fill = "Year", x = "", y = "") +
    ggplot2::scale_fill_manual(
      values = setNames(
        c(pal[["light_text"]], pal[["dark_text"]]),
        c(year1, year2)
      )
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 500)) +
    ggplot2::scale_x_discrete(
      labels = c(
        "Canned" = "Canned",
        "Fillet" = "Fillet",
        "Fresh" = "Fresh",
        "Frozen" = "Frozen",
        "Headed-and-gutted" = "Headed and gutted",
        "Other" = "Other",
        "Unprocessed" = "Unprocessed",
        "Smoked" = "Smoked"
      )
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme(
      # note: need to make these fills as transparent for final figure when stitching together
      # they are colored right now so we can show the bacgkround color in the individual plot
      panel.background = ggplot2::element_rect(
        fill = pal[["bg_plot"]],
        color = pal[["bg_plot"]]
      ),
      plot.background = ggplot2::element_rect(
        fill = pal[["bg_plot"]],
        color = pal[["bg_plot"]]
      ),
      # axis
      axis.line.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        color = pal["value1"],
        # family = "sen",
        size = 18,
        hjust = 1,
        margin = ggplot2::margin(-10, -20, -50, 0)
      ),
      axis.text.x = ggplot2::element_text(
        color = pal["value1"],
        size = 18
      ),
      # axis.text.x = element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      # axis.ticks.x = ggplot2::element_line(color = pal["dark_text"]),

      # grids
      panel.grid.major.x = ggplot2::element_line(color = pal[["value2"]]),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      # legend
      legend.position = "none"
    )

  return(barchart)
}


############################### lolipop chart  ##################################

lollipop_func <- function(data, year1, year2) {
  ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = value,
      y = forcats::fct_reorder(variable, value),
      group = variable,
      color = factor(year)
    )
  ) +
    # Draw segments connecting the two years for each variable
    ggplot2::geom_line(
      aes(group = variable),
      color = pal[["value2"]],
      linewidth = 1
    ) +

    # Add points for each year
    ggplot2::geom_point(size = 5) +

    ggplot2::labs(color = "Year", x = "", y = "") +

    ggplot2::scale_color_manual(
      values = setNames(
        c(pal[["light_text"]], pal[["dark_text"]]),
        c(year1, year2)
      )
    ) +

    ggplot2::scale_x_continuous(limits = c(0, 780)) +

    ggplot2::scale_y_discrete(
      labels = c(
        "All production" = "All production",
        "Groundfish production" = "Groundfish production",
        "Pacific whiting" = "Pacific whiting",
        "Non-whiting groundfish" = "Non-whiting groundfish",
        "Sablefish" = "Sablefish",
        "Rockfish" = "Rockfish",
        "Dover sole" = "Dover sole",
        "Petrale sole" = "Petrale sole",
        "Thornyheads" = "Thornyheads",
        "Other groundfish species" = "Other groundfish species",
        "Other species production" = "Other species production",
        "Crab" = "Crab",
        "Shrimp" = "Shrimp",
        "Salmon" = "Salmon",
        "Tuna" = "Tuna",
        "Coastal pelagics" = "Coastal pelagics",
        "Other shellfish" = "Other shellfish",
        "Other species" = "Other species"
      )
    ) +

    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = pal[["bg_plot"]],
        color = pal[["bg_plot"]]
      ),
      plot.background = ggplot2::element_rect(
        fill = pal[["bg_plot"]],
        color = pal[["bg_plot"]]
      ),
      axis.line.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        color = pal["value1"],
        size = 18,
        hjust = 1,
        margin = margin(-10, -20, -50, 0)
      ),
      axis.text.x = ggplot2::element_text(
        color = pal["value1"],
        size = 18
      ),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = pal[["value2"]]),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    )
}


############################## Explore the Data plots ##################################

plot_func <- function(data, lab, group, facet, title = NULL) {
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
    ggplot2::aes(x = factor(year), y = value, group = .data[[group]])
  ) +
    scale_fill_manual(values = line_col) +
    scale_color_manual(values = line_col) +
    scale_linetype_manual(values = line_ty) +
    theme_minimal() +
    labs(
      y = lab,
      x = "Year",
      title = title
    ) +
    scale_x_discrete(breaks = scales::pretty_breaks()) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
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
      axis.line = element_line(color = "grey", linewidth = 1), # Adds borders to only x and y axes
      panel.background = ggplot2::element_rect(
        fill = pal[["bg_plot"]],
        color = pal[["bg_plot"]]
      ),
      plot.background = ggplot2::element_rect(
        fill = pal[["bg_plot"]],
        color = pal[["bg_plot"]]
      )
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
  cols_to_remove <- c("ylab", "tab", "unit_lab")

  df |>
    dplyr::select(-any_of(cols_to_remove)) |>
    dplyr::mutate(
      variance = round(variance, 2),
      q25 = round(q25, 2),
      q75 = round(q75, 2),
      value = round(value, 2),
      lower = round(lower, 2),
      upper = round(upper, 2)
    )
}
