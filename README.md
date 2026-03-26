# STAT 5243 Project 2 — Team 24

**Universal Data Preprocessing & EDA App**

Deployed Application: https://jiqiucu.shinyapps.io/STAT5243_Team24_Final_App/

## Overview

An interactive R Shiny web application for data uploading, cleaning, feature engineering, and exploratory data analysis. Users can upload datasets in multiple formats, apply preprocessing steps, engineer new features, and explore the data through interactive visualizations — all without writing code.

## How to Run

1. Clone this repository
2. Open `app.R` in RStudio
3. Install required packages:
```r
install.packages(c("shiny", "DT", "dplyr", "readr", "readxl", "jsonlite",
                    "lubridate", "geosphere", "ggplot2"))
```
4. Click **Run App** or run in console:
```r
shiny::runApp()
```
5. Upload a data file or use the built-in Citi Bike dataset

## App Structure

The app has five main tabs:

**Home / Guide** — Instructions on supported file formats and how to use the app.

**Data Loading** — Upload CSV, TSV, TXT, JSON, XLSX, RDS, or Parquet files. Built-in datasets (Citi Bike, iris) available for demo. Shows raw data preview, schema profile, and quality summary.

**Data Cleaning** — Interactive preprocessing: standardize column names, handle missing values (drop, median, mean, zero-fill), remove duplicates, handle outliers (IQR clipping, winsorization), scale numeric columns (z-score, min-max). Transformation log tracks all changes. Download cleaned data as CSV or JSON.

**Feature Engineering** — Select from 11 engineered features in three categories: time-based (trip duration, hour, day of week, weekend flag, time segment, week of month), geospatial (haversine distance, average speed, round-trip flag), and rider/bike type (electric flag, member binary). Adjustable sample size. Results shown across Feature Guide, Data Preview, Distributions, Scatter/Correlation, and Summary Statistics tabs.

**EDA** — Six interactive sub-tabs for exploring the processed data:
- Overview: dataset dimensions, column types, variable type table
- Distribution: histogram, density, or boxplot for any numeric variable
- Scatter / Correlation: scatter plot with optional color grouping and trend line, Pearson correlation
- Data Preview: interactive DT table of the current dataset
- Filter & Explore: dynamic sliders and dropdowns to filter rows in real time, with CSV download
- Group Compare: group by categorical variable, aggregate numeric metric (mean, median, sum, count, SD), bar chart + summary table

## File Structure

```
├── app.R                              # Main Shiny application (all modules integrated)
├── EDAproject2.R                      # Standalone EDA module (development version)
├── feature.R                          # Feature engineering script
├── mod_clean_ui.R                     # Data cleaning UI module
├── mod_clean_server.R                 # Data cleaning server module
├── utils_clean.R                      # Cleaning utility functions
├── JC-202301-citibike-tripdata.csv    # Built-in Citi Bike sample dataset
├── sample_clean.csv                   # Sample cleaned dataset
├── project 2 uiux.Rproj              # RStudio project file
└── README.md
```

## Dependencies

- R >= 4.0
- shiny, DT, dplyr, readr, readxl, jsonlite, lubridate, geosphere, ggplot2

## Team Contributions

| Member | Role |
|--------|------|
| Isaac Beers | Data upload: multi-format file reading, schema profiling, quality summary |
| Ji Qiu | Data cleaning and preprocessing: missing values, outliers, scaling, type conversion |
| Selina Peng | Feature engineering: 11 engineered features, interactive selection, edge-case handling |
| Zuer Weng | EDA: 6 sub-tabs (overview, distribution, scatter/correlation, data preview, filter, group compare) |
| Wenyang Zu | UI/UX design: layout, styling, navigation, responsiveness, user guide |
