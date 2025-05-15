#' plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#'@import ggplot2
#'
#' @noRd

# this script contains all the code for creating plots/tables for the app including their aesthetics

########################### Plot Aesthetics ###########################

# see data_processing.R scipt where they are made
# then stored in sysdata.R file
# ex: pal[], line_ty, line_col,

############################### lolipop chart  ##################################

# this function creates lollipop charts that are used on the "Overview" page

lollipop_func <- function(data, year1, range1, range2, upper_lim) {
  # range label for the graph legend
  # if year range is the same (e.g. 2020-2020 avg., then making it show just 2020)
  range_label <- if (range1 != range2) {
    paste0(range1, "-", range2, " avg.")
  } else {
    range2
  }

  # factor the year so year 1 always shows up first and same color on graph
  data$year <- factor(
    data$year,
    levels = unique(c(range_label, as.character(year1)))
  )

  # ggplot graph code
  ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = .data[["value"]],
      y = forcats::fct_reorder(
        .data[["variable"]],
        .data[["value"]]
      ),
      group = .data[["variable"]],
      color = factor(.data[["year"]])
    )
  ) +
    # Draw segments connecting the two years for each variable
    ggplot2::geom_line(
      aes(group = .data[["variable"]]),
      color = pal[["value2"]],
      linewidth = 1
    ) +

    # Add points for each year
    ggplot2::geom_point(size = 5) +

    ggplot2::labs(color = "Year", x = "", y = "") +

    ggplot2::scale_color_manual(
      values = stats::setNames(
        c(pal[["light_text"]], pal[["dark_text"]]),
        c(as.character(year1), range_label)
      )
    ) +

    ggplot2::scale_x_continuous(limits = c(0, upper_lim)) +

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

      # legend
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      legend.text = element_text(size = 18, color = pal["value1"]), # <-- Adjust size here
      legend.background = element_rect(fill = pal[["bg_plot"]], color = NA),
      legend.box.background = element_rect(fill = pal[["bg_plot"]], color = NA)
    )
}


############################## Line Graph ##################################

# this function creates line graphs that are used in the "Plot" tab on the "Explore the Data" page

plot_func <- function(data, lab, group, facet, line = "solid", title = NULL) {
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
    ggplot2::aes(
      x = factor(.data[["year"]]),
      y = .data[["value"]],
      group = .data[[group]]
    )
  ) +
    geom_point(aes(color = .data[[group]]), size = 4) +
    geom_line(
      aes(
        color = .data[[group]],
        linetype = .data[[group]]
      ),
      linewidth = 0.75
    ) +
    # geom_ribbon(
    #   aes(
    #     ymax = .data[["upper"]],
    #     ymin = .data[["lower"]],
    #     fill = .data[[group]]
    #   ),
    #   alpha = .2
    # ) +
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
    scale_y_continuous(breaks = scales::pretty_breaks(), expand = c(0, 0)) +
    theme(
      text = element_text(size = 22, color = pal["value1"]),
      axis.text = element_text(size = 18, color = pal["value1"]),
      strip.text = element_text(size = 18, color = pal["value1"]),
      legend.text = element_text(color = pal["value1"]),
      legend.title = element_blank(),
      legend.position = "bottom", # Moves the legend to the bottom
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(linewidth = 1.2),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(linewidth = 1.2),
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
    # facet wrap based on the column specified to be faceted in the function
    ggplot2::facet_wrap(
      stats::as.formula(base::paste("~", facet)),
      scales = 'free_y',
      ncol = 2
    )
}

############################## Data Table render processing ##################################

# function for cleaning up data frame to be rendered under the "Table" panel of "Explore the Data" page
process_df <- function(df) {
  # list of columns to remove that are not needed
  cols_to_remove <- c("ylab", "tab", "unit_lab")

  df |>
    # remove cols if they exist
    dplyr::select(-dplyr::any_of(cols_to_remove)) |> # remove cols if they exist
    # round numbers
    dplyr::mutate(
      variance = round(.data[["variance"]], 2),
      q25 = round(.data[["q25"]], 2),
      q75 = round(.data[["q75"]], 2),
      value = round(.data[["value"]], 2),
      lower = round(.data[["lower"]], 2),
      upper = round(.data[["upper"]], 2)
    )
}
