#' data_cleaning
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

########################### Cleaning Raw data #################################

raw_purcprod <- readRDS("data/mini_purcprod.RDS")

clean_purcprod <- raw_purcprod |>
  # making all variable names lowercase
  janitor::clean_names() |>
  # reducing values by units
  dplyr::mutate(
    variance = dplyr::case_when(
      unit == '' ~ variance,
      unit == 'thousands' ~ variance / 1e3,
      unit == 'millions' ~ variance / 1e6,
      unit == 'billions' ~ variance / 1e9,
      T ~ -999
    ),
    q25 = dplyr::case_when(
      unit == '' ~ q25,
      unit == 'thousands' ~ q25 / 1e3,
      unit == 'millions' ~ q25 / 1e6,
      unit == 'billions' ~ q25 / 1e9,
      T ~ -999
    ),
    q75 = dplyr::case_when(
      unit == '' ~ q75,
      unit == 'thousands' ~ q75 / 1e3,
      unit == 'millions' ~ q75 / 1e6,
      unit == 'billions' ~ q75 / 1e9,
      T ~ -999
    ),
    value = dplyr::case_when(
      unit == '' ~ value,
      unit == 'thousands' ~ value / 1e3,
      unit == 'millions' ~ value / 1e6,
      unit == 'billions' ~ value / 1e9,
      T ~ -999
    ),
    upper = dplyr::case_when(
      statistic == 'Mean' ~ value + variance,
      statistic == 'Median' ~ q75,
      statistic == 'Total' ~ value
    ),
    lower = dplyr::case_when(
      statistic == 'Mean' ~ value - variance,
      statistic == 'Median' ~ q25,
      statistic == 'Total' ~ value
    )
  ) |>
  dplyr::mutate(
    # new labels for plots
    unit_lab = stringr::str_replace(
      ylab,
      "(?<=:)(.*)",
      ~ stringr::str_remove_all(.x, "(Mean|Median|Total|[()])")
    ) |>
      stringr::str_squish()
  ) |>
  data.frame()

# Data formatting for table#####
data_table <- clean_purcprod |>
  dplyr::mutate(
    variance = round(variance, 2),
    q25 = round(q25, 2),
    q75 = round(q75, 2),
    value = round(value, 2)
  ) |>
  data.frame()


####################### Cleaning "Summary" tab data ###########################

# subsetting data for the "Summary" tab
sumdf <- dplyr::filter(clean_purcprod, tab == 'Summary')

# subsetting data for the "Production Activities" subtab under "Summary"
sumdf_prac <- sumdf |>
  dplyr::filter(cs == "")

# subsetting data for the "Region" subtab under "Summary"
sumdf_reg <- sumdf |>
  dplyr::filter(
    cs != "",
    variable %in% c("California", "Washington and Oregon")
  )

# subsetting data for the "Processor size/type" subtab under "Summary"
sumdf_size <- sumdf |>
  dplyr::filter(
    cs != "",
    variable %in% c("Small", "Medium", "Large", "Non-processor")
  )


####################### Cleaning "By Product Type" tab data ###########################

# subsetting data for the "By Product Type" tab
proddf <- dplyr::filter(clean_purcprod, tab == 'Product') |> # production tab data
  tidyr::separate(
    metric,
    into = c("type", "metric"),
    sep = " ",
    extra = "merge"
  ) |>
  dplyr::mutate(metric = substr(metric, 2, nchar(metric) - 1))

# subsetting data for the "Production Activities" subtab under "By Product Type"
proddf_prac <- proddf |>
  dplyr::filter(cs == "")

# subsetting data for the "Region" subtab under "By Product Type"
proddf_reg <- proddf |>
  dplyr::filter(
    cs != "",
    variable %in% c("California", "Washington and Oregon")
  )

# subsetting data for the "Processor size/type" subtab under "By Product Type"
proddf_size <- proddf |>
  dplyr::filter(
    cs != "",
    variable %in% c("Small", "Medium", "Large", "Non-processor")
  )


####################### Cleaning "By Product Type" tab data ###########################

specsdf <- raw_purcprod |>
  dplyr::select(-c(ylab)) |>
  janitor::clean_names() |>
  dplyr::filter(
    tab == 'Product', # production tab data
    cs == ""
  ) |>

  tidyr::separate(
    metric,
    into = c("type", "metric"),
    sep = " ",
    extra = "merge"
  ) |>
  dplyr::mutate(metric = substr(metric, 2, nchar(metric) - 1)) |>

  # original data filter already classified labels as millions, billions,
  dplyr::mutate(
    unit = dplyr::case_when(
      metric == "Production weight" ~ "millions",
      metric == "Production value" ~ "millions",
      metric == "Production price (per lb)" ~ ""
    ),

    q25 = dplyr::case_when(
      metric == "Production weight" ~ q25 / 1e6,
      metric == "Production value" ~ q25 / 1e6,
      metric == "Production price (per lb)" ~ q25
    ),

    q75 = dplyr::case_when(
      metric == "Production weight" ~ q75 / 1e6,
      metric == "Production value" ~ q75 / 1e6,
      metric == "Production price (per lb)" ~ q75
    ),

    value = dplyr::case_when(
      metric == "Production weight" ~ value / 1e6,
      metric == "Production value" ~ value / 1e6,
      metric == "Production price (per lb)" ~ value
    ),
    variance = dplyr::case_when(
      metric == "Production weight" ~ variance / 1e6,
      metric == "Production value" ~ variance / 1e6,
      metric == "Production price (per lb)" ~ variance
    ),
    upper = dplyr::case_when(
      statistic == 'Mean' ~ value + variance,
      statistic == 'Median' ~ q75,
      statistic == 'Total' ~ value
    ),
    lower = dplyr::case_when(
      statistic == 'Mean' ~ value - variance,
      statistic == 'Median' ~ q25,
      statistic == 'Total' ~ value
    ),
    unit_lab = paste0(variable, " (", metric, "): ", unit, " nominal $")
  )
# dplyr::mutate(
#   unit_lab = paste0(variable, " (", metric, "): ", unit, " nominal $")
# )

thornyheads <- specsdf |>
  dplyr::filter(
    variable == "Thornyheads",
    statistic == "Median",
    metric == "Production weight"
  )

other_gs <- specsdf |>
  dplyr::filter(
    # tab == "Product",
    variable == "Other groundfish species",
    statistic == "Mean",
    metric == "Production weight",
    type == "Frozen"
  )
# metric == "Production weight")

grouped <- specsdf |>
  dplyr::filter(metric == "Production weight", statistic == "Mean") |>
  dplyr::group_by(variable, type, year) |>
  dplyr::summarize(mean_new = mean(value))


####################### helper function to process data ############################
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
