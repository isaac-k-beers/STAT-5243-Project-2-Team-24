# STAT 5243 Project 2 — Team 24

## Exploratory Data Analysis (EDA) Module

### Overview
The EDA module is integrated into the main `app.R` Shiny application. It provides interactive tools for exploring and analyzing uploaded datasets after preprocessing. The module reads the cleaned data from the P2 Preprocess step and offers five functional tabs.

### EDA Features

**EDA Overview**
- Displays dataset dimensions (rows and columns)
- Shows column type breakdown (numeric, categorical, date/time)
- Reports missing value counts and percentages
- Generates summary statistics tables for both numeric and categorical columns (mean, SD, min, Q1, median, Q3, max for numeric; unique count, top value, top frequency for categorical)

**EDA Distributions**
- Supports four plot types: Histogram, Density, Boxplot, and Bar Chart
- Users can select any variable and adjust histogram bin count
- Grouping and color coding by categorical variables
- Optional outlier capping at 1st/99th percentile
- Automatically switches to Bar Chart for categorical variables

**EDA Correlation**
- Interactive correlation heatmap with three methods: Pearson, Spearman, and Kendall
- Users can select/deselect numeric columns for the heatmap
- Scatter plot with customizable X/Y axes and color grouping
- Optional linear trend line overlay
- Adjustable point opacity for dense data

**EDA Filter & Explore**
- Dynamically generated filters based on column types (sliders for numeric, dropdowns for categorical)
- Real-time row count display showing filtered vs total rows
- Filtered data preview table
- Download filtered data as CSV
- Reset all filters button

**EDA Group Compare**
- Group data by any categorical variable
- Choose a numeric metric and an aggregation function (Mean, Median, Sum, Count, Std Dev)
- Horizontal bar chart visualization with viridis color scale
- Summary table with group values and counts

### How to Run
1. Open `app.R` in RStudio
2. Make sure the required packages are installed: `shiny`, `DT`, `dplyr`, `readr`, `readxl`, `ggplot2`, `jsonlite`, `reshape2`
3. Click **Run App** or run in console:
   ```r
   shiny::runApp("app.R")
   ```
4. Upload a data file (CSV, TSV, TXT, JSON, XLSX, RDS, or Parquet)
5. Navigate to the EDA tabs to explore the data

### Dependencies
- R >= 4.0
- shiny
- DT
- dplyr
- readr
- readxl
- ggplot2
- jsonlite
- reshape2
