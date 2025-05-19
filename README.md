
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` -->
<!-- up-to-date. `devtools::build_readme()` is handy for this. -->

# purcprod

**An app built by NOAA for exploring economic data of US West Coast
fisheries**

## 🚀 Launch the app here: [link]()

<!-- badges: start -->

[![R-CMD-check](https://github.com/ramhunte/purchprod/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ramhunte/purchprod/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

------------------------------------------------------------------------

## 🧭 About the app

This Shiny App (built in R) is a tool that was developed to assist the
Economic Data Collection Program at the
<a href="https://www.fisheries.noaa.gov/about/northwest-fisheries-science-center" target="_blank">Northwest
Fisheries Science Center</a> make its fisheries data available to the
public. Data is collected annually from the West Coast Groundfish Trawl
Fishery as required by
<a href="https://www.ecfr.gov/current/title-50/chapter-VI/part-660/subpart-D/section-660.114" target="_blank">regulation
50 CFR 660.114</a>. This app aggregates, summarizes, and compares
various facets of this data pertaining to the purchase and production of
the West Coast Groundfish Trawl Catch Share Program.

### Notes for Developers

This app was built using the `{golem}` framework, which structures Shiny
applications as R packages to support modular design, easier testing,
and long-term maintainability and collaboration. The app’s
components—UI, server logic, and supporting functions—are organized into
clearly separated scripts (`fct_*.R`, `utils_*.R`) and modules
(`mod_*.R`), making the codebase easier to navigate, test, and extend.
Configuration settings are managed through the `golem-config.yml` file,
which centralizes environment-specific options. To ensure
reproducibility across systems and time, the project also uses `{renv}`
to lock package versions. The `renv.lock` file captures the exact
versions of all packages used during development, allowing collaborators
to recreate the same environment with `renv::restore()`. If contributing
to this application, please follow the `{golem}` and `{renv}`
conventions to maintain consistency and reliability throughout
development.

------------------------------------------------------------------------

## ✨ Features

### **Overview page**

- A broad look at species catch value and weight by year
- Compare total production value and weight of species over time
- Select a specific year of interest and compare it to another year or
  average over multiple years
  - Use the **year selector** and **date range slider** to customize the
    comparison

### **Explore the Data**

- A more granular look into the facets of the data with tabs including:
  - **Summary**
  - **By Product Type**
  - **By Species**
- Filter by a metric, statistic, region, processor type, and more
- View output as interactive time series plots or downloadable tables

### **Information**

- Learn how the app works and understand key terms and filters

### **Contact us**

- Get in touch with NOAA developers or app managers for support or
  suggestions

<!-- -->

    .
    ├── DESCRIPTION 
    ├── LICENSE 
    ├── LICENSE.md 
    ├── NAMESPACE 
    ├── R # R scripts including UI, server, modules (mod_*.R), functions (fct_*.R, utils_ui.R), and app configuration.
    │   ├── app_config.R # reads and applies global settings from YAML config
    │   ├── app_server.R
    │   ├── app_ui.R
    │   ├── fct_footer.R # function that creates the footer
    │   ├── fct_header.R # function that creates the header
    │   ├── fct_plot.R # functions that create plots
    │   ├── mod_other_tabs.R 
    │   ├── mod_overview.R
    │   ├── mod_prod_type.R
    │   ├── mod_specs.R
    │   ├── mod_specs_tabs.R
    │   ├── mod_summary.R
    │   ├── run_app.R
    │   ├── sysdata.rda # internal data used by app but not exported
    │   └── utils_ui.R # helper functions that are mostly reusable (used in the UI)
    ├── README.Rmd
    ├── README.md
    ├── data-raw # contains data processing script and raw data (for development only)
    │   ├── data_processing.R
    │   └── mini_purcprod.RDS
    ├── dev # development scripts to set up, build, and deploy the app (made by {golem})
    │   ├── 01_start.R
    │   ├── 02_dev.R
    │   ├── 03_deploy.R
    │   ├── config_attachment.yaml
    │   └── run_dev.R
    ├── inst # non-code files needed by the app
    │   ├── WORDLIST
    │   ├── app # Contains markdown files, static images, and CSS file
    │   │   ├── text # markdown files for text on "Information" and "Contact Us" pages
    │   │   │   ├── contact.md
    │   │   │   └── info.md
    │   │   └── www # header image and CSS file
    │   │       ├── noaa_header.png
    │   │       └── styles.css
    │   └── golem-config.yml # global configuration options from {golem} framework
    ├── man # includes Rd documentation and README figure assets
    │   ├── figures
    │   │   └── nmfs_logo.png
    │   └── run_app.Rd
    ├── purcprod.Rproj
    ├── renv # managed by {Renv} to isolate package dependencies and create reproducible environment
    │   ├── activate.R
    │   ├── settings.json
    │   └── staging
    ├── renv.lock # specifies dependencies and versions for development and time of of deployment
    └── tests # tests to make sure app is functioning as intended using {testthat}
        ├── spelling.R
        ├── testthat
        │   ├── test-app.R
        │   ├── test-app_server.R
        │   ├── test-app_ui.R
        │   ├── test-other_tabs.R
        │   ├── test-overview.R
        │   ├── test-plot.R
        │   ├── test-summary.R
        │   └── test-utils_ui.R
        └── testthat.R

------------------------------------------------------------------------

## 📊 Data

The app uses data collected annually from participants in the West Coast
Groundfish Trawl Fishery. As of **April 2025**, the app contains data
through the **2023 calendar year**.

Data collection is part of NOAA’s [Economic Data Collection
Program](https://www.fisheries.noaa.gov/west-coast/science-data/economic-data-collection-west-coast-groundfish-trawl-fishery).

------------------------------------------------------------------------

## 📬 Send us a message!

We welcome feedback, suggestions, and questions regarding the app and
data.

📧 <a href="mailto:nmfs.nwfsc.fisheye@noaa.gov"
class="email"><strong>nmfs.nwfsc.fisheye@noaa.gov</strong></a>

------------------------------------------------------------------------

<img src="man/figures/nmfs_logo.png" alt="NOAA Fisheries Logo" width="200" style="height: 75px !important;"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
