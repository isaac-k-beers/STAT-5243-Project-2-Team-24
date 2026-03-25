library(shiny)
library(DT)

multi_check_block <- function(ns, label_text, input_id, all_id, none_id) {
  tagList(
    tags$label(label_text),
    fluidRow(
      column(
        width = 6,
        actionButton(ns(all_id), "Select All", width = "100%")
      ),
      column(
        width = 6,
        actionButton(ns(none_id), "Clear All", width = "100%")
      )
    ),
    checkboxGroupInput(
      ns(input_id),
      label = NULL,
      choices = NULL,
      selected = character(0)
    ),
    br()
  )
}

mod_clean_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("General Data Cleaning & Preprocessing"),
    
    fluidRow(
      column(
        width = 4,
        
        checkboxInput(ns("remove_dupes"), "Remove duplicate rows", FALSE),
        br(),
        
        multi_check_block(ns, "Columns to keep (leave empty = keep all)",
                          "keep_cols", "keep_all", "keep_none"),
        
        multi_check_block(ns, "Columns to parse as datetime",
                          "datetime_cols", "datetime_all", "datetime_none"),
        
        multi_check_block(ns, "Columns to convert to factor",
                          "factor_cols", "factor_all", "factor_none"),
        
        multi_check_block(ns, "Columns to convert to numeric",
                          "numeric_cols", "numeric_all", "numeric_none"),
        
        multi_check_block(ns, "Columns to convert to character",
                          "character_cols", "character_all", "character_none"),
        
        selectInput(
          ns("missing_method"),
          "Missing value handling",
          choices = c(
            "None" = "none",
            "Drop rows with missing values" = "drop_rows",
            "Median/Mode imputation" = "median_mode"
          ),
          selected = "none"
        ),
        br(),
        
        multi_check_block(ns, "Columns for missing value handling (leave empty = all columns)",
                          "missing_cols", "missing_all", "missing_none"),
        
        checkboxInput(ns("do_scale"), "Scale numeric columns", FALSE),
        br(),
        
        multi_check_block(ns, "Numeric columns to scale",
                          "scale_cols", "scale_all", "scale_none"),
        
        checkboxInput(ns("remove_outliers"), "Remove outliers by IQR", FALSE),
        br(),
        
        multi_check_block(ns, "Numeric columns for outlier removal",
                          "outlier_cols", "outlier_all", "outlier_none"),
        
        downloadButton(ns("download_cleaned"), "Download Cleaned Data")
      ),
      
      column(
        width = 8,
        h4("Before Cleaning"),
        tableOutput(ns("overview_before")),
        DTOutput(ns("missing_before")),
        
        h4("After Cleaning"),
        tableOutput(ns("overview_after")),
        DTOutput(ns("missing_after"))
      )
    ),
    
    hr(),
    
    h4("Cleaned Data Preview"),
    DTOutput(ns("clean_preview"))
  )
}
