# STAT 5243 Project 2 — Team 24  
## Data Cleaning and Preprocessing Module

### Overview
The Data Cleaning and Preprocessing module is integrated into the main `app.R` Shiny application. It provides interactive tools for preparing general tabular datasets before downstream analysis. The module reads the uploaded or built-in dataset, allows users to perform common cleaning and preprocessing operations, and outputs a cleaned dataset that can be passed to later modules such as feature engineering and exploratory data analysis.

### Data Cleaning and Preprocessing Features

#### Data Overview
- Displays dataset dimensions (rows and columns)
- Reports the number of duplicate rows
- Shows variable types for all columns
- Reports missing value counts and percentages by column
- Provides before-and-after summaries so users can compare the dataset before and after preprocessing

#### Column Selection
- Allows users to select which columns to retain
- Supports flexible subsetting so users can focus only on variables relevant to their analysis

#### Duplicate Removal
- Identifies duplicated observations
- Removes duplicate rows when the user selects this option

#### Data Type Conversion
- Allows selected variables to be converted into:
  - Datetime
  - Numeric
  - Factor
  - Character
- Supports flexible handling of different tabular datasets rather than one specific data source

#### Missing Value Handling
- Supports three strategies:
  - No treatment
  - Drop rows with missing values
  - Median/mode imputation
- Applies missing-value handling either to selected columns or to the full dataset

#### Numeric Scaling
- Standardizes selected numeric variables
- Improves comparability across variables with different scales

#### Outlier Handling
- Removes extreme observations using the interquartile range (IQR) rule
- Can be applied to selected numeric columns

#### Real-Time Feedback
- Updates dataset summaries immediately after each selected operation
- Displays a cleaned data preview table in real time

#### Data Export
- Allows users to download the cleaned dataset as a CSV file
- Supports downstream tasks such as feature engineering, visualization, and modeling

### How to Run
1. Open `app.R` in RStudio  
2. Make sure the required packages are installed: `shiny`, `DT`, `dplyr`, `tidyr`  
3. Click **Run App** or run the following command in the console:

```r
shiny::runApp()
