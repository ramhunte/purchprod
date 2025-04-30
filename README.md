
<!-- README.md is generated from README.Rmd. Please edit that file -->

# purchprod

**An app built by NOAA for exploring economic data of US West Coast
fisheries**

## 🚀 Launch the app here: [link]()

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/ramhunte/purchprod/graph/badge.svg)](https://app.codecov.io/gh/ramhunte/purchprod)
[![R-CMD-check](https://github.com/ramhunte/purchprod/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ramhunte/purchprod/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

------------------------------------------------------------------------

## 🧭 About the app

This Shiny App (built in R) is a tool that was developed to assist the
Economic Data Collection Program at the
<a href="https://www.fisheries.noaa.gov/about/northwest-fisheries-science-center" target="_blank">Northwest
Fisheries Science Center</a> make its economic data available to the
public. Data is collected annually from the West Coast Groundfish Trawl
Fishery as required by
<a href="https://www.ecfr.gov/current/title-50/chapter-VI/part-660/subpart-D/section-660.114" target="_blank">regulation
50 CFR 660.114</a>. This app aggregates, summarizes, and compares
various facets of this data pertaining to the purchase and production of
the West Coast Groundfish Trawl Catch Share Program.

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

**Contact us** - Get in touch with NOAA developers or app managers for
support or suggestions

------------------------------------------------------------------------

## 📊 Data

The app uses data collected annually from participants in the West Coast
Groundfish Trawl Fishery.  
As of **April 2025**, the app contains data through the **2023 calendar
year**.

Data collection is part of NOAA’s [Economic Data Collection
Program](https://www.fisheries.noaa.gov/west-coast/science-data/economic-data-collection-west-coast-groundfish-trawl-fishery).

------------------------------------------------------------------------

## 📬 Send us a message!

We welcome feedback, suggestions, and questions regarding the app and
data.

📧 **<nmfs.nwfsc.fisheye@noaa.gov>**

------------------------------------------------------------------------

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.
