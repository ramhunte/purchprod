## this script processes the raw data stored in the data-raw file
# it is used for development purposes only. I take the raw data (raw_purcprod), clean it (clean_purcprod)
# and then subset it into smaller data frames for individual tabs and their associated plots
# I intentionally do not save raw_purcprod, clean_purcprod, nor proddf to the 'data' folder
# as they are not called in the app itself. just used in this script for cleaning

#NOTE: this script does not need to be run again unless there are changes to the raw data in the future

########################### Reading  Raw data #################################

raw_purcprod <- readRDS("data-raw/mini_purcprod.RDS")

########################### Cleaning Raw data #################################

# this df is a cleaned version of the raw data
clean_purcprod <- raw_purcprod |>
  # making all variable names lowercase for consistency
  janitor::clean_names() |>
  # adjusting raw values by their assigned units
  # ex: if a value is labeled "millions", then we divide it by 1,000,000 to make it abbreviated
  # do this for variance, quartiles, value, upper/lower
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
    # new labels for plot facets
    unit_lab = stringr::str_replace(
      ylab,
      "(?<=:)(.*)",
      # removing statistic
      ~ stringr::str_remove_all(.x, "(Mean|Median|Total|[()])")
    ) |>
      # removing extra blank space
      stringr::str_squish()
  ) |>
  data.frame()

####################### Cleaning "Summary" tab data ###########################
# this section subsets the data to be used for the "Summary" tab on the "Explore the Data" page

# subsetting data for the "Production Activities" bottom tab under "Summary"
sumdf_prac <- clean_purcprod |>
  dplyr::filter(tab == 'Summary', cs == "")

# subsetting data for the "Region" bottom tab under "Summary"
sumdf_reg <- clean_purcprod |>
  dplyr::filter(
    tab == 'Summary',
    cs != "",
    variable %in% c("California", "Washington and Oregon")
  )

# subsetting data for the "Processor size/type" bottom tab under "Summary"
sumdf_size <- clean_purcprod |>
  dplyr::filter(
    tab == 'Summary',
    cs != "",
    variable %in% c("Small", "Medium", "Large", "Non-processor")
  )


####################### Cleaning "By Product Type" tab data ###########################

# this section subsets the data to be used for the "By Product Type" tab on the "Explore the Data" page

# cleaning data for the "By Product Type" tab
proddf <- dplyr::filter(clean_purcprod, tab == 'Product') |> # production tab data
  tidyr::separate(
    metric,
    into = c("type", "metric"),
    sep = " ",
    extra = "merge"
  ) |>
  dplyr::mutate(metric = substr(metric, 2, nchar(metric) - 1))

# subsetting data for the "Production Activities" bottom tab under "By Product Type"
proddf_prac <- proddf |>
  dplyr::filter(cs == "")

# subsetting data for the "Region" bottom tab under "By Product Type"
proddf_reg <- proddf |>
  dplyr::filter(
    cs != "",
    variable %in% c("California", "Washington and Oregon")
  )

# subsetting data for the "Processor size/type" bottom tab under "By Product Type"
proddf_size <- proddf |>
  dplyr::filter(
    cs != "",
    variable %in% c("Small", "Medium", "Large", "Non-processor")
  )


####################### Cleaning "Species" tab data ###########################

# this data set is going to be similar to the By Product Type tab but flipped. However,
# some data labeled a "thousands" and some as "millions" will be put on the same plot
# here (unlike in the previous data frames), so I am restructuring raw data so it is
# all shown as numbers in the millions and not also thousands.

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

####################### writing to data folder ###########################

# this function writes the desired data frames that are used in the app into the 'data' folder
usethis::use_data(
  ########### for "Summary" tab on the Explore the Data page
  sumdf_prac,
  sumdf_reg,
  sumdf_size,
  ###########  for "By Product Type" tab on the Explore the Data page
  proddf_prac,
  proddf_reg,
  proddf_size,
  ###########  for "By Species" tab on the Explore the Data page
  specsdf,
  overwrite = TRUE
)
