library(shiny)
library(DT)
library(readr)
library(readxl)
library(jsonlite)
library(dplyr)
library(lubridate)
library(geosphere)
library(ggplot2)

# --- source existing files ---
source("dt_ft_R/mod_clean_ui.R")
source("dt_ft_R/mod_clean_server.R")
source("dt_ft_R/utils_clean.R")

# =========================
# Helpers for general loading
# =========================

guess_txt_delim <- function(path) {
  lines <- readLines(path, n = 5, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nchar(lines) > 0]
  if (length(lines) == 0) return(NA_character_)
  
  line <- lines[1]
  delims <- c("," = ",", "\t" = "\t", ";" = ";", "|" = "|")
  counts <- sapply(delims, function(d) length(strsplit(line, d, fixed = TRUE)[[1]]) - 1)
  
  if (max(counts) <= 0) return(NA_character_)
  names(which.max(counts))
}

coerce_to_df <- function(obj) {
  if (inherits(obj, "data.frame")) return(as.data.frame(obj, stringsAsFactors = FALSE))
  if (is.matrix(obj)) return(as.data.frame(obj, stringsAsFactors = FALSE))
  
  if (is.atomic(obj) && is.null(dim(obj))) {
    return(data.frame(value = obj, stringsAsFactors = FALSE))
  }
  
  if (is.list(obj)) {
    if (length(obj) == 0) return(data.frame())
    
    direct_df <- tryCatch(
      as.data.frame(obj, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (!is.null(direct_df)) return(direct_df)
    
    row_list <- lapply(obj, function(el) {
      if (inherits(el, "data.frame")) return(el)
      
      if (is.list(el)) {
        tmp <- tryCatch(
          as.data.frame(el, stringsAsFactors = FALSE),
          error = function(e) NULL
        )
        if (!is.null(tmp)) return(tmp)
        
        return(data.frame(
          raw_json = jsonlite::toJSON(el, auto_unbox = TRUE),
          stringsAsFactors = FALSE
        ))
      }
      
      data.frame(value = as.character(el), stringsAsFactors = FALSE)
    })
    
    out <- tryCatch(dplyr::bind_rows(row_list), error = function(e) NULL)
    if (!is.null(out)) return(as.data.frame(out, stringsAsFactors = FALSE))
    
    return(data.frame(
      raw_json = jsonlite::toJSON(obj, auto_unbox = TRUE),
      stringsAsFactors = FALSE
    ))
  }
  
  stop("Unsupported object type.")
}

flatten_list_cols <- function(df) {
  for (nm in names(df)) {
    if (is.list(df[[nm]])) {
      df[[nm]] <- vapply(df[[nm]], function(x) {
        if (length(x) == 0 || all(is.na(x))) return(NA_character_)
        jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
      }, character(1))
    }
  }
  df
}

read_json_as_df <- function(path) {
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  obj <- jsonlite::fromJSON(txt, flatten = TRUE, simplifyVector = TRUE)
  df <- coerce_to_df(obj)
  flatten_list_cols(df)
}

read_txt_as_df <- function(path) {
  delim <- guess_txt_delim(path)
  
  if (is.na(delim)) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    return(data.frame(
      line_id = seq_along(lines),
      text = lines,
      stringsAsFactors = FALSE
    ))
  }
  
  out <- tryCatch(
    readr::read_delim(path, delim = delim, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(out)) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    data.frame(
      line_id = seq_along(lines),
      text = lines,
      stringsAsFactors = FALSE
    )
  } else {
    as.data.frame(out, stringsAsFactors = FALSE)
  }
}

read_any_file <- function(path, ext) {
  ext <- tolower(ext)
  
  df <- switch(
    ext,
    "csv"  = as.data.frame(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), stringsAsFactors = FALSE),
    "tsv"  = as.data.frame(readr::read_tsv(path, show_col_types = FALSE, progress = FALSE), stringsAsFactors = FALSE),
    "txt"  = read_txt_as_df(path),
    "json" = read_json_as_df(path),
    "xlsx" = as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE),
    "xls"  = as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE),
    "rds"  = coerce_to_df(readRDS(path)),
    stop(paste("Unsupported file type:", ext))
  )
  
  flatten_list_cols(df)
}

# =========================
# Feature engineering helpers
# =========================

feature_descriptions <- c(
  trip_duration_min     = "Minutes elapsed between ride start and end times.",
  hour_of_day           = "Hour (0–23) when the ride started.",
  day_of_week           = "Full weekday name when the trip started.",
  is_weekend            = "Whether the trip started on a weekday or weekend.",
  time_of_day_segment   = "Morning Rush, Midday, Evening Rush, or Night/Off-peak.",
  week_of_month         = "Which week of the month the trip falls in.",
  haversine_distance_km = "Straight-line distance between start and end coordinates.",
  avg_speed_kmh         = "Distance divided by duration in hours.",
  same_station_trip     = "Whether the trip started and ended at the same station.",
  is_electric           = "Electric vs non-electric bike label.",
  member_binary         = "1 for member, 0 for casual rider."
)

feature_labels <- c(
  "Trip Duration (minutes)" = "trip_duration_min",
  "Hour of Day" = "hour_of_day",
  "Day of Week" = "day_of_week",
  "Weekday / Weekend" = "is_weekend",
  "Time-of-Day Segment" = "time_of_day_segment",
  "Week of Month" = "week_of_month",
  "Haversine Distance (km)" = "haversine_distance_km",
  "Average Speed (km/h)" = "avg_speed_kmh",
  "Round-Trip Flag" = "same_station_trip",
  "Electric Bike Flag" = "is_electric",
  "Member (binary 0/1)" = "member_binary"
)

numeric_features <- c(
  "trip_duration_min", "hour_of_day", "week_of_month",
  "haversine_distance_km", "avg_speed_kmh", "member_binary"
)

feature_requirements <- list(
  trip_duration_min     = c("started_at", "ended_at"),
  hour_of_day           = c("started_at"),
  day_of_week           = c("started_at"),
  is_weekend            = c("started_at"),
  time_of_day_segment   = c("started_at"),
  week_of_month         = c("started_at"),
  haversine_distance_km = c("start_lat", "start_lng", "end_lat", "end_lng"),
  avg_speed_kmh         = c("started_at", "ended_at", "start_lat", "start_lng", "end_lat", "end_lng"),
  same_station_trip     = c("start_station_id", "end_station_id"),
  is_electric           = c("rideable_type"),
  member_binary         = c("member_casual")
)

available_features_for_df <- function(df) {
  cols <- names(df)
  valid <- vapply(names(feature_requirements), function(f) {
    all(feature_requirements[[f]] %in% cols)
  }, logical(1))
  names(valid)[valid]
}

engineer_features_safe <- function(df, selected_features) {
  out <- df
  
  if ("started_at" %in% names(out)) {
    out$started_at <- suppressWarnings(lubridate::ymd_hms(out$started_at))
    if (all(is.na(out$started_at))) {
      out$started_at <- suppressWarnings(as.POSIXct(out$started_at))
    }
  }
  
  if ("ended_at" %in% names(out)) {
    out$ended_at <- suppressWarnings(lubridate::ymd_hms(out$ended_at))
    if (all(is.na(out$ended_at))) {
      out$ended_at <- suppressWarnings(as.POSIXct(out$ended_at))
    }
  }
  
  if ("trip_duration_min" %in% selected_features &&
      all(c("started_at", "ended_at") %in% names(out))) {
    out <- out %>%
      mutate(trip_duration_min = as.numeric(difftime(ended_at, started_at, units = "mins")))
  }
  
  if ("hour_of_day" %in% selected_features &&
      "started_at" %in% names(out)) {
    out <- out %>% mutate(hour_of_day = hour(started_at))
  }
  
  if ("day_of_week" %in% selected_features &&
      "started_at" %in% names(out)) {
    out <- out %>% mutate(day_of_week = wday(started_at, label = TRUE, abbr = FALSE))
  }
  
  if ("is_weekend" %in% selected_features &&
      "started_at" %in% names(out)) {
    out <- out %>%
      mutate(is_weekend = if_else(wday(started_at) %in% c(1, 7), "Weekend", "Weekday"))
  }
  
  if ("time_of_day_segment" %in% selected_features &&
      "started_at" %in% names(out)) {
    out <- out %>%
      mutate(time_of_day_segment = case_when(
        hour(started_at) >= 6  & hour(started_at) < 10 ~ "Morning Rush",
        hour(started_at) >= 10 & hour(started_at) < 16 ~ "Midday",
        hour(started_at) >= 16 & hour(started_at) < 20 ~ "Evening Rush",
        TRUE ~ "Night/Off-peak"
      ))
  }
  
  if ("week_of_month" %in% selected_features &&
      "started_at" %in% names(out)) {
    out <- out %>% mutate(week_of_month = ceiling(day(started_at) / 7))
  }
  
  if ("haversine_distance_km" %in% selected_features &&
      all(c("start_lat", "start_lng", "end_lat", "end_lng") %in% names(out))) {
    out <- out %>%
      rowwise() %>%
      mutate(
        haversine_distance_km = if_else(
          !is.na(start_lat) & !is.na(start_lng) & !is.na(end_lat) & !is.na(end_lng),
          geosphere::distHaversine(c(start_lng, start_lat), c(end_lng, end_lat)) / 1000,
          NA_real_
        )
      ) %>%
      ungroup()
  }
  
  if ("avg_speed_kmh" %in% selected_features &&
      all(c("started_at", "ended_at", "start_lat", "start_lng", "end_lat", "end_lng") %in% names(out))) {
    
    if (!"trip_duration_min" %in% names(out)) {
      out <- out %>%
        mutate(trip_duration_min = as.numeric(difftime(ended_at, started_at, units = "mins")))
    }
    
    if (!"haversine_distance_km" %in% names(out)) {
      out <- out %>%
        rowwise() %>%
        mutate(
          haversine_distance_km = if_else(
            !is.na(start_lat) & !is.na(start_lng) & !is.na(end_lat) & !is.na(end_lng),
            geosphere::distHaversine(c(start_lng, start_lat), c(end_lng, end_lat)) / 1000,
            NA_real_
          )
        ) %>%
        ungroup()
    }
    
    out <- out %>%
      mutate(
        avg_speed_kmh = if_else(
          trip_duration_min > 0,
          haversine_distance_km / (trip_duration_min / 60),
          NA_real_
        )
      )
  }
  
  if ("same_station_trip" %in% selected_features &&
      all(c("start_station_id", "end_station_id") %in% names(out))) {
    out <- out %>%
      mutate(
        same_station_trip = if_else(
          !is.na(start_station_id) & !is.na(end_station_id) &
            start_station_id == end_station_id,
          "Round Trip", "One-way"
        )
      )
  }
  
  if ("is_electric" %in% selected_features &&
      "rideable_type" %in% names(out)) {
    out <- out %>%
      mutate(is_electric = if_else(rideable_type == "electric_bike", "Electric", "Non-electric"))
  }
  
  if ("member_binary" %in% selected_features &&
      "member_casual" %in% names(out)) {
    out <- out %>%
      mutate(member_binary = if_else(member_casual == "member", 1L, 0L))
  }
  
  out
}

# =========================
# UI
# =========================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f7f9fc;
        font-family: Arial, sans-serif;
      }
      .hero {
        background: #1f4e79;
        color: white;
        padding: 20px 24px;
        border-radius: 10px;
        margin-bottom: 18px;
      }
      .card {
        background: white;
        border-radius: 10px;
        padding: 18px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        margin-bottom: 16px;
      }
      .note {
        color: #555;
        font-size: 14px;
      }
      .status-box {
        background: #f8f9fa;
        border: 1px solid #dfe3e8;
        border-radius: 8px;
        padding: 12px;
        white-space: pre-wrap;
        font-family: Arial, sans-serif;
        font-size: 14px;
      }
    "))
  ),
  
  div(
    class = "hero",
    h2("Project 2 UI/UX App"),
    p("Interactive workflow for data cleaning, feature engineering, and EDA.")
  ),
  
  tabsetPanel(
    id = "main_tabs",
    
    tabPanel(
      "Home / Guide",
      br(),
      div(
        class = "card",
        h3("Welcome"),
        p("This app is designed to provide a clear and user-friendly workflow for Project 2."),
        tags$ul(
          tags$li("Data Loading: upload or use a built-in dataset."),
          tags$li("Data Cleaning: general preprocessing tools for uploaded or provided data."),
          tags$li("Feature Engineering: currently optimized for Citi Bike trip data."),
          tags$li("EDA: interactive exploration of the current dataset.")
        ),
        p("Recommended workflow:"),
        tags$ol(
          tags$li("Start with a built-in dataset or upload your own file."),
          tags$li("Review the dataset summary and preview in Data Loading."),
          tags$li("Apply cleaning and preprocessing."),
          tags$li("Use feature engineering if the required Citi Bike columns are available."),
          tags$li("Explore the processed data in EDA.")
        ),
        p(
          class = "note",
          "Note: This UI/UX demo uses Citi Bike as the default sample dataset. ",
          "The loading and cleaning workflow is more general; feature engineering currently requires specific trip-related columns."
        )
      )
    ),
    
    tabPanel(
      "Data Loading",
      br(),
      div(
        class = "card",
        h3("Load Dataset"),
        p("Choose a built-in dataset or upload your own file."),
        p(
          class = "note",
          "Supported formats: CSV, TSV, TXT, JSON, XLSX, XLS, and RDS. ",
          "The default demo dataset is Citi Bike."
        ),
        
        fluidRow(
          column(
            4,
            radioButtons(
              "data_source",
              "Select data source:",
              choices = c(
                "Built-in dataset" = "builtin",
                "Upload file" = "upload"
              ),
              selected = "builtin"
            ),
            
            conditionalPanel(
              condition = "input.data_source == 'builtin'",
              selectInput(
                "builtin_data",
                "Choose built-in dataset:",
                choices = c("Citi Bike" = "citibike")
              )
            ),
            
            conditionalPanel(
              condition = "input.data_source == 'upload'",
              fileInput(
                "user_file",
                "Upload dataset",
                accept = c(".csv", ".tsv", ".txt", ".json", ".xlsx", ".xls", ".rds")
              )
            )
          ),
          
          column(
            8,
            h4("Dataset Summary"),
            tableOutput("load_info"),
            br(),
            h4("Preview"),
            DTOutput("load_preview"),
            br(),
            h4("Status"),
            verbatimTextOutput("load_msg")
          )
        )
      )
    ),
    
    tabPanel(
      "Data Cleaning",
      br(),
      div(
        class = "card",
        h3("Cleaning Module"),
        p(
          class = "note",
          "Use this tab to remove duplicates, handle missing values, convert types, scale numeric columns, and remove outliers."
        ),
        mod_clean_ui("clean1")
      )
    ),
    
    tabPanel(
      "Feature Engineering",
      br(),
      div(
        class = "card",
        h3("Feature Engineering"),
        p("This module generates new derived variables from the current cleaned dataset."),
        p(
          class = "note",
          "This module works best for Citi Bike trip data. Only features whose required columns are available will be shown."
        ),
        
        fluidRow(
          column(
            4,
            h4("Select Features"),
            uiOutput("feature_picker_ui"),
            br(),
            actionButton("apply_features", "Apply Feature Engineering", class = "btn btn-primary"),
            br(), br(),
            downloadButton("download_engineered", "Download Engineered Data")
          ),
          
          column(
            8,
            h4("Feature Guide"),
            tableOutput("feature_guide_table"),
            br(),
            h4("Status"),
            div(class = "status-box", textOutput("feature_status"))
          )
        ),
        hr(),
        h4("Engineered Data Preview"),
        DTOutput("feature_preview")
      )
    ),
    
    tabPanel(
      "EDA",
      br(),
      div(
        class = "card",
        h3("Exploratory Data Analysis"),
        p("Explore the current processed dataset interactively."),
        p(
          class = "note",
          "You can switch between cleaned data and engineered data, inspect summary statistics, view distributions, and explore relationships between variables."
        ),
        
        fluidRow(
          column(
            3,
            radioButtons(
              "eda_source",
              "Choose dataset for EDA:",
              choices = c(
                "Cleaned Data" = "cleaned",
                "Engineered Data" = "engineered"
              ),
              selected = "engineered"
            ),
            
            hr(),
            
            h4("Distribution Plot"),
            selectInput("eda_dist_var", "Numeric variable:", choices = NULL),
            selectInput(
              "eda_dist_type",
              "Plot type:",
              choices = c("Histogram", "Density", "Boxplot"),
              selected = "Histogram"
            ),
            
            hr(),
            
            h4("Scatter Plot"),
            selectInput("eda_x", "X variable:", choices = NULL),
            selectInput("eda_y", "Y variable:", choices = NULL),
            selectInput("eda_color", "Colour by:", choices = "None")
          ),
          
          column(
            9,
            tabsetPanel(
              tabPanel(
                "Overview",
                br(),
                h4("Dataset Summary"),
                tableOutput("eda_overview"),
                br(),
                h4("Variable Types"),
                tableOutput("eda_types")
              ),
              
              tabPanel(
                "Distribution",
                br(),
                plotOutput("eda_dist_plot", height = "420px"),
                br(),
                verbatimTextOutput("eda_dist_summary")
              ),
              
              tabPanel(
                "Scatter / Correlation",
                br(),
                plotOutput("eda_scatter_plot", height = "420px"),
                br(),
                verbatimTextOutput("eda_corr_text")
              ),
              
              tabPanel(
                "Data Preview",
                br(),
                DTOutput("eda_preview")
              ),
              
              tabPanel(
                "Filter & Explore",
                br(),
                fluidRow(
                  column(
                    3,
                    h4("Filters"),
                    uiOutput("eda_filter_ui"),
                    br(),
                    actionButton("eda_reset_filters", "Reset All Filters",
                                 class = "btn-warning btn-sm")
                  ),
                  column(
                    9,
                    h4("Filtered Data"),
                    uiOutput("eda_filter_count"),
                    DTOutput("eda_filtered_table"),
                    br(),
                    downloadButton("eda_download_filtered", "Download Filtered CSV")
                  )
                )
              ),
              
              tabPanel(
                "Group Compare",
                br(),
                fluidRow(
                  column(
                    3,
                    selectInput("eda_grp_var", "Group by (categorical):", choices = NULL),
                    selectInput("eda_grp_metric", "Metric (numeric):", choices = NULL),
                    selectInput("eda_grp_func", "Aggregation:",
                                choices = c("Mean", "Median", "Sum", "Count", "Std Dev"))
                  ),
                  column(
                    9,
                    h4("Grouped Summary"),
                    plotOutput("eda_grp_plot", height = "420px"),
                    br(),
                    DTOutput("eda_grp_table")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# =========================
# Server
# =========================

server <- function(input, output, session) {
  
  raw_data <- reactive({
    if (input$data_source == "builtin") {
      if (input$builtin_data == "citibike") {
        read.csv("JC-202301-citibike-tripdata.csv", stringsAsFactors = FALSE)
      } else {
        NULL
      }
    } else {
      req(input$user_file)
      ext <- tools::file_ext(input$user_file$name)
      as.data.frame(read_any_file(input$user_file$datapath, ext), stringsAsFactors = FALSE)
    }
  })
  
  output$load_info <- renderTable({
    req(raw_data())
    df <- raw_data()
    
    data.frame(
      Metric = c("Rows", "Columns"),
      Value = c(nrow(df), ncol(df))
    )
  })
  
  output$load_preview <- renderDT({
    req(raw_data())
    datatable(
      head(raw_data(), 20),
      options = list(scrollX = TRUE, pageLength = 5)
    )
  })
  
  output$load_msg <- renderText({
    req(raw_data())
    
    if (input$data_source == "builtin") {
      paste0(
        "Using built-in dataset: Citi Bike\n",
        "Rows: ", nrow(raw_data()), "\n",
        "Columns: ", ncol(raw_data())
      )
    } else {
      paste0(
        "Uploaded file: ", input$user_file$name, "\n",
        "Rows: ", nrow(raw_data()), "\n",
        "Columns: ", ncol(raw_data())
      )
    }
  })
  
  clean_mod <- mod_clean_server("clean1", data_r = raw_data)
  
  feature_input_data <- reactive({
    req(clean_mod$data())
    clean_mod$data()
  })
  
  output$feature_picker_ui <- renderUI({
    req(feature_input_data())
    available <- available_features_for_df(feature_input_data())
    
    if (length(available) == 0) {
      return(tags$p(
        "No Citi Bike-compatible feature options are available for the current dataset.",
        class = "note"
      ))
    }
    
    pretty_choices <- feature_labels[feature_labels %in% available]
    
    checkboxGroupInput(
      "selected_features",
      label = NULL,
      choices = pretty_choices,
      selected = intersect(
        c("trip_duration_min", "is_weekend", "haversine_distance_km"),
        available
      )
    )
  })
  
  output$feature_guide_table <- renderTable({
    req(feature_input_data())
    available <- available_features_for_df(feature_input_data())
    
    if (length(available) == 0) return(NULL)
    
    data.frame(
      Feature = available,
      Type = ifelse(available %in% numeric_features, "Numeric", "Categorical"),
      Description = unname(feature_descriptions[available]),
      row.names = NULL
    )
  })
  
  engineered_data <- eventReactive(input$apply_features, {
    req(feature_input_data())
    
    available <- available_features_for_df(feature_input_data())
    selected <- input$selected_features
    
    if (is.null(selected) || length(selected) == 0) {
      return(feature_input_data())
    }
    
    selected_valid <- intersect(selected, available)
    
    if (length(selected_valid) == 0) {
      return(feature_input_data())
    }
    
    engineer_features_safe(feature_input_data(), selected_valid)
  }, ignoreNULL = FALSE)
  
  output$feature_status <- renderText({
    req(feature_input_data())
    available <- available_features_for_df(feature_input_data())
    
    if (length(available) == 0) {
      return("No feature-engineering options are available for the current dataset.")
    }
    
    selected <- input$selected_features
    selected_valid <- intersect(selected, available)
    
    paste(
      paste0("Available features (", length(available), "):"),
      paste(available, collapse = ", "),
      "",
      if (length(selected_valid) == 0) {
        "Selected features: none"
      } else {
        paste0("Selected features: ", paste(selected_valid, collapse = ", "))
      },
      sep = "\n"
    )
  })
  
  output$feature_preview <- renderDT({
    req(engineered_data())
    datatable(
      head(engineered_data(), 20),
      options = list(scrollX = TRUE, pageLength = 5)
    )
  })
  
  output$download_engineered <- downloadHandler(
    filename = function() {
      paste0("engineered_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(engineered_data(), file, row.names = FALSE)
    }
  )
  
  # =========================
  # EDA data source
  # =========================
  
  eda_data <- reactive({
    if (input$eda_source == "cleaned") {
      req(clean_mod$data())
      clean_mod$data()
    } else {
      req(engineered_data())
      engineered_data()
    }
  })
  
  observe({
    req(eda_data())
    df <- eda_data()
    
    num_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    
    updateSelectInput(
      session, "eda_dist_var",
      choices = num_cols,
      selected = if (length(num_cols) > 0) num_cols[1] else character(0)
    )
    
    updateSelectInput(
      session, "eda_x",
      choices = num_cols,
      selected = if (length(num_cols) > 0) num_cols[1] else character(0)
    )
    
    updateSelectInput(
      session, "eda_y",
      choices = num_cols,
      selected = if (length(num_cols) > 1) num_cols[2] else if (length(num_cols) == 1) num_cols[1] else character(0)
    )
    
    updateSelectInput(
      session, "eda_color",
      choices = c("None", cat_cols),
      selected = "None"
    )
    
    updateSelectInput(
      session, "eda_grp_var",
      choices = cat_cols,
      selected = if (length(cat_cols) > 0) cat_cols[1] else character(0)
    )
    
    updateSelectInput(
      session, "eda_grp_metric",
      choices = num_cols,
      selected = if (length(num_cols) > 0) num_cols[1] else character(0)
    )
  })
  
  output$eda_overview <- renderTable({
    req(eda_data())
    df <- eda_data()
    
    data.frame(
      Metric = c("Rows", "Columns", "Numeric Columns", "Categorical Columns"),
      Value = c(
        nrow(df),
        ncol(df),
        sum(sapply(df, is.numeric)),
        sum(sapply(df, function(x) is.character(x) || is.factor(x)))
      )
    )
  })
  
  output$eda_types <- renderTable({
    req(eda_data())
    df <- eda_data()
    
    data.frame(
      Variable = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      row.names = NULL
    )
  })
  
  output$eda_dist_plot <- renderPlot({
    req(eda_data(), input$eda_dist_var)
    df <- eda_data()
    var <- input$eda_dist_var
    req(var %in% names(df))
    
    plot_df <- df %>%
      filter(!is.na(.data[[var]]))
    
    req(nrow(plot_df) > 0)
    
    if (input$eda_dist_type == "Histogram") {
      ggplot(plot_df, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = "#4C78A8", alpha = 0.8) +
        theme_minimal(base_size = 13) +
        labs(title = paste("Histogram of", var), x = var, y = "Count")
    } else if (input$eda_dist_type == "Density") {
      ggplot(plot_df, aes(x = .data[[var]])) +
        geom_density(fill = "#72B7B2", alpha = 0.5) +
        theme_minimal(base_size = 13) +
        labs(title = paste("Density of", var), x = var, y = "Density")
    } else {
      ggplot(plot_df, aes(y = .data[[var]])) +
        geom_boxplot(fill = "#F58518", alpha = 0.7) +
        theme_minimal(base_size = 13) +
        labs(title = paste("Boxplot of", var), y = var, x = "")
    }
  })
  
  output$eda_dist_summary <- renderPrint({
    req(eda_data(), input$eda_dist_var)
    df <- eda_data()
    var <- input$eda_dist_var
    req(var %in% names(df))
    
    cat("Summary of", var, ":\n")
    print(summary(df[[var]]))
  })
  
  output$eda_scatter_plot <- renderPlot({
    req(eda_data(), input$eda_x, input$eda_y)
    df <- eda_data()
    x <- input$eda_x
    y <- input$eda_y
    col_var <- input$eda_color
    
    req(x %in% names(df), y %in% names(df))
    
    plot_df <- df %>%
      filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
    
    req(nrow(plot_df) > 0)
    
    if (!is.null(col_var) && col_var != "None" && col_var %in% names(plot_df)) {
      ggplot(plot_df, aes(x = .data[[x]], y = .data[[y]], color = .data[[col_var]])) +
        geom_point(alpha = 0.5, size = 1.6) +
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        theme_minimal(base_size = 13) +
        labs(title = paste(y, "vs", x), x = x, y = y, color = col_var)
    } else {
      ggplot(plot_df, aes(x = .data[[x]], y = .data[[y]])) +
        geom_point(alpha = 0.5, size = 1.6, color = "#4C78A8") +
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        theme_minimal(base_size = 13) +
        labs(title = paste(y, "vs", x), x = x, y = y)
    }
  })
  
  output$eda_corr_text <- renderPrint({
    req(eda_data(), input$eda_x, input$eda_y)
    df <- eda_data()
    x <- input$eda_x
    y <- input$eda_y
    
    req(x %in% names(df), y %in% names(df))
    
    pair_df <- df %>%
      select(all_of(c(x, y))) %>%
      filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
    
    if (nrow(pair_df) < 3) {
      cat("Not enough complete observations to compute correlation.\n")
    } else {
      corr <- cor(pair_df[[x]], pair_df[[y]])
      cat("Correlation between", x, "and", y, ":\n")
      print(round(corr, 4))
    }
  })
  
  output$eda_preview <- renderDT({
    req(eda_data())
    datatable(
      head(eda_data(), 30),
      options = list(scrollX = TRUE, pageLength = 6)
    )
  })
  
  # =========================
  # EDA: Filter & Explore
  # =========================
  
  eda_num_cols <- reactive({
    df <- req(eda_data())
    names(df)[sapply(df, is.numeric)]
  })
  
  eda_cat_cols <- reactive({
    df <- req(eda_data())
    names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  })
  
  output$eda_filter_ui <- renderUI({
    df <- req(eda_data())
    nc <- eda_num_cols()
    cc <- eda_cat_cols()
    widgets <- list()
    
    for (col in head(nc, 6)) {
      rng <- range(df[[col]], na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2]) && rng[1] < rng[2]) {
        widgets[[col]] <- sliderInput(
          paste0("eda_filt_num_", col), label = col,
          min = rng[1], max = rng[2], value = rng,
          step = signif((rng[2] - rng[1]) / 100, 2)
        )
      }
    }
    for (col in head(cc, 6)) {
      lvls <- sort(unique(as.character(df[[col]][!is.na(df[[col]])])))
      if (length(lvls) > 0 && length(lvls) <= 50) {
        widgets[[col]] <- selectInput(
          paste0("eda_filt_cat_", col), label = col,
          choices = c("(All)" = "__ALL__", lvls),
          selected = "__ALL__", multiple = TRUE
        )
      }
    }
    if (length(widgets) == 0) return(tags$p("No filterable columns detected."))
    do.call(tagList, widgets)
  })
  
  observeEvent(input$eda_reset_filters, {
    df <- eda_data()
    nc <- eda_num_cols()
    cc <- eda_cat_cols()
    for (col in head(nc, 6)) {
      rng <- range(df[[col]], na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2]))
        updateSliderInput(session, paste0("eda_filt_num_", col), value = rng)
    }
    for (col in head(cc, 6))
      updateSelectInput(session, paste0("eda_filt_cat_", col), selected = "__ALL__")
  })
  
  eda_filtered <- reactive({
    df <- req(eda_data())
    nc <- eda_num_cols()
    cc <- eda_cat_cols()
    for (col in head(nc, 6)) {
      val <- input[[paste0("eda_filt_num_", col)]]
      if (!is.null(val) && length(val) == 2)
        df <- df[!is.na(df[[col]]) & df[[col]] >= val[1] & df[[col]] <= val[2], , drop = FALSE]
    }
    for (col in head(cc, 6)) {
      val <- input[[paste0("eda_filt_cat_", col)]]
      if (!is.null(val) && !("__ALL__" %in% val))
        df <- df[as.character(df[[col]]) %in% val, , drop = FALSE]
    }
    df
  })
  
  output$eda_filter_count <- renderUI({
    orig <- nrow(req(eda_data()))
    filt <- nrow(eda_filtered())
    tags$p(tags$b("Showing: "),
           format(filt, big.mark = ","), " / ",
           format(orig, big.mark = ","), " rows",
           style = "color: #2c3e50; font-size: 14px;")
  })
  
  output$eda_filtered_table <- renderDT({
    datatable(head(eda_filtered(), 500), rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 15, dom = "tip"))
  })
  
  output$eda_download_filtered <- downloadHandler(
    filename = function() paste0("filtered_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(eda_filtered(), file, row.names = FALSE)
  )
  
  # =========================
  # EDA: Group Compare
  # =========================
  
  eda_grp_summary <- reactive({
    df <- req(eda_data())
    gvar <- req(input$eda_grp_var)
    mvar <- req(input$eda_grp_metric)
    func <- req(input$eda_grp_func)
    req(gvar %in% names(df), mvar %in% names(df))
    
    df <- df[!is.na(df[[gvar]]) & !is.na(df[[mvar]]), , drop = FALSE]
    df[[gvar]] <- factor(df[[gvar]])
    
    agg_fn <- switch(func,
      "Mean"    = function(x) round(mean(x, na.rm = TRUE), 4),
      "Median"  = function(x) round(median(x, na.rm = TRUE), 4),
      "Sum"     = function(x) round(sum(x, na.rm = TRUE), 4),
      "Count"   = function(x) length(x),
      "Std Dev" = function(x) round(sd(x, na.rm = TRUE), 4)
    )
    
    df %>%
      group_by(Group = .data[[gvar]]) %>%
      summarise(Value = agg_fn(.data[[mvar]]), N = n(), .groups = "drop") %>%
      arrange(desc(Value))
  })
  
  output$eda_grp_plot <- renderPlot({
    result <- req(eda_grp_summary())
    gvar <- input$eda_grp_var
    mvar <- input$eda_grp_metric
    func <- input$eda_grp_func
    
    ggplot(result, aes(x = reorder(Group, Value), y = Value, fill = Group)) +
      geom_col(alpha = 0.85, show.legend = FALSE) +
      coord_flip() +
      theme_minimal(base_size = 14) +
      labs(title = paste(func, "of", mvar, "by", gvar), x = gvar,
           y = paste(func, "of", mvar)) +
      scale_fill_viridis_d(option = "D")
  })
  
  output$eda_grp_table <- renderDT({
    datatable(eda_grp_summary(), rownames = FALSE,
              options = list(scrollX = TRUE, dom = "tip"))
  })
}

shinyApp(ui, server)
