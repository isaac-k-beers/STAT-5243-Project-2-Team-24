
required_pkgs <- c("shiny", "dplyr", "lubridate", "geosphere",
                   "ggplot2", "DT", "shinydashboard")

new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")

library(shiny)
library(dplyr)
library(lubridate)
library(geosphere)
library(ggplot2)
library(DT)
library(shinydashboard)


engineer_features <- function(df, selected_features) {

  df <- df %>%
    mutate(
      started_at = ymd_hms(started_at),
      ended_at   = ymd_hms(ended_at)
    )

  if ("trip_duration_min" %in% selected_features)
    df <- df %>%
      mutate(trip_duration_min = as.numeric(difftime(ended_at, started_at, units = "mins")))

  if ("hour_of_day" %in% selected_features)
    df <- df %>% mutate(hour_of_day = hour(started_at))

  if ("day_of_week" %in% selected_features)
    df <- df %>% mutate(day_of_week = wday(started_at, label = TRUE, abbr = FALSE))

  if ("is_weekend" %in% selected_features)
    df <- df %>%
      mutate(is_weekend = if_else(wday(started_at) %in% c(1, 7), "Weekend", "Weekday"))

  if ("time_of_day_segment" %in% selected_features)
    df <- df %>%
      mutate(time_of_day_segment = case_when(
        hour(started_at) >= 6  & hour(started_at) < 10  ~ "Morning Rush",
        hour(started_at) >= 10 & hour(started_at) < 16  ~ "Midday",
        hour(started_at) >= 16 & hour(started_at) < 20  ~ "Evening Rush",
        TRUE                                              ~ "Night/Off-peak"
      ))

  if ("week_of_month" %in% selected_features)
    df <- df %>% mutate(week_of_month = ceiling(day(started_at) / 7))

  if ("haversine_distance_km" %in% selected_features)
    df <- df %>%
      rowwise() %>%
      mutate(
        haversine_distance_km = if_else(
          !is.na(start_lat) & !is.na(end_lat),
          distHaversine(c(start_lng, start_lat), c(end_lng, end_lat)) / 1000,
          NA_real_
        )
      ) %>%
      ungroup()

  if ("avg_speed_kmh" %in% selected_features) {
    if (!"haversine_distance_km" %in% names(df))
      df <- df %>%
        rowwise() %>%
        mutate(haversine_distance_km = if_else(
          !is.na(start_lat) & !is.na(end_lat),
          distHaversine(c(start_lng, start_lat), c(end_lng, end_lat)) / 1000,
          NA_real_)) %>%
        ungroup()
    if (!"trip_duration_min" %in% names(df))
      df <- df %>%
        mutate(trip_duration_min = as.numeric(difftime(ended_at, started_at, units = "mins")))
    df <- df %>%
      mutate(avg_speed_kmh = if_else(
        trip_duration_min > 0,
        haversine_distance_km / (trip_duration_min / 60),
        NA_real_
      ))
  }

  if ("same_station_trip" %in% selected_features)
    df <- df %>%
      mutate(same_station_trip = if_else(
        !is.na(start_station_id) & !is.na(end_station_id) &
          start_station_id == end_station_id,
        "Round Trip", "One-way"
      ))

  if ("is_electric" %in% selected_features)
    df <- df %>%
      mutate(is_electric = if_else(rideable_type == "electric_bike", "Electric", "Non-electric"))

  if ("member_binary" %in% selected_features)
    df <- df %>%
      mutate(member_binary = if_else(member_casual == "member", 1L, 0L))

  df
}

feature_catalogue <- list(
  `⏱ Time-Based` = c(
    "Trip Duration (minutes)"  = "trip_duration_min",
    "Hour of Day"              = "hour_of_day",
    "Day of Week"              = "day_of_week",
    "Weekday / Weekend"        = "is_weekend",
    "Time-of-Day Segment"      = "time_of_day_segment",
    "Week of Month"            = "week_of_month"
  ),
  `🗺 Geospatial` = c(
    "Haversine Distance (km)"  = "haversine_distance_km",
    "Average Speed (km/h)"     = "avg_speed_kmh",
    "Round-Trip Flag"          = "same_station_trip"
  ),
  `🚲 Rider & Bike` = c(
    "Electric Bike Flag"       = "is_electric",
    "Member (binary 0/1)"      = "member_binary"
  )
)

feature_descriptions <- c(
  trip_duration_min     = "Minutes elapsed between ride start and end times.",
  hour_of_day           = "Hour (0–23) when the ride started.",
  day_of_week           = "Full weekday name (Monday … Sunday).",
  is_weekend            = "Weekday vs Weekend based on the start date.",
  time_of_day_segment   = "Morning Rush (6–10), Midday (10–16), Evening Rush (16–20), or Night.",
  week_of_month         = "Which week of the month (1–5) the ride falls in.",
  haversine_distance_km = "Straight-line distance between start and end GPS coordinates.",
  avg_speed_kmh         = "Distance ÷ duration in hours; a proxy for riding speed.",
  same_station_trip     = "Whether the ride starts and ends at the same station (Round Trip).",
  is_electric           = "Electric vs non-electric bike label.",
  member_binary         = "1 = annual member, 0 = casual rider."
)

numeric_features <- c("trip_duration_min", "haversine_distance_km",
                       "avg_speed_kmh", "hour_of_day", "week_of_month", "member_binary")
ui <- fluidPage(
  title = "Citi Bike Feature Engineering",

  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', sans-serif; background: #f4f6f9; }
    .navbar-title { font-weight: 700; font-size: 1.3em; }
    .sidebar-panel { background: #ffffff; border-radius: 8px;
                     padding: 16px; box-shadow: 0 1px 4px rgba(0,0,0,.08); }
    .main-panel { background: #ffffff; border-radius: 8px;
                  padding: 20px; box-shadow: 0 1px 4px rgba(0,0,0,.08); }
    .btn-primary { background-color: #0d6efd; border: none; width: 100%; }
    .badge-count { background: #0d6efd; color: white; border-radius: 12px;
                   padding: 2px 8px; font-size: .8em; margin-left: 6px; }
    hr { border-color: #e9ecef; }
  "))),

  tags$div(
    style = "background:#1a1a2e; color:white; padding:14px 24px; margin-bottom:20px;
             border-radius:0 0 8px 8px; display:flex; align-items:center; gap:12px;",
    tags$span("🚲", style = "font-size:1.8em;"),
    tags$div(
      tags$h4("Citi Bike — Feature Engineering", style = "margin:0; font-weight:700;"),
      tags$small("Jersey City | January 2023 | 56,075 trips", style = "opacity:.7;")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "sidebar-panel",

        h5("📂 Data Source", style = "font-weight:600;"),
        fileInput("upload_csv", label = NULL,
                  accept = ".csv",
                  placeholder = "Upload your own CSV…",
                  buttonLabel = "Browse"),
        helpText("Leave blank to use the built-in Jan 2023 Citi Bike dataset."),
        hr(),

        h5("🛠 Features to Engineer", style = "font-weight:600;"),
        checkboxGroupInput(
          "selected_feats", label = NULL,
          choices  = feature_catalogue,
          selected = c("trip_duration_min", "is_weekend",
                       "time_of_day_segment", "haversine_distance_km")
        ),
        hr(),

        h5("🔢 Sample Size", style = "font-weight:600;"),
        sliderInput("sample_n", label = NULL,
                    min = 500, max = 56075, value = 10000, step = 500,
                    post = " rows"),
        br(),
        actionButton("run_btn", "▶  Apply Features",
                     class = "btn btn-primary"),
        br(), br(),
        downloadButton("download_btn", "⬇  Download CSV",
                       style = "width:100%;")
      )
    ),

    mainPanel(
      width = 9,
      div(class = "main-panel",
        tabsetPanel(
          id = "main_tabs",

          # Tab 1 — Guide
          tabPanel("📖 Feature Guide", br(),
            uiOutput("feature_guide_ui")
          ),

          # Tab 2 — Preview
          tabPanel("🔍 Data Preview", br(),
            DTOutput("preview_table")
          ),

          tabPanel("📊 Distributions", br(),
            fluidRow(
              column(4, selectInput("plot_feature", "Numeric feature:", choices = NULL)),
              column(4, selectInput("plot_group", "Colour / group by:",
                                   choices = c("None", "is_weekend",
                                               "time_of_day_segment",
                                               "is_electric", "same_station_trip",
                                               "member_casual"))),
              column(4, selectInput("plot_type", "Plot type:",
                                   choices = c("Histogram", "Density", "Boxplot")))
            ),
            plotOutput("dist_plot", height = "400px"),
            br(),
            verbatimTextOutput("dist_summary")
          ),

          tabPanel("🔗 Scatter", br(),
            fluidRow(
              column(4, selectInput("scatter_x", "X axis:", choices = NULL)),
              column(4, selectInput("scatter_y", "Y axis:", choices = NULL)),
              column(4, selectInput("scatter_color", "Colour by:",
                                   choices = c("None", "is_weekend",
                                               "time_of_day_segment",
                                               "is_electric", "member_casual")))
            ),
            plotOutput("scatter_plot", height = "420px")
          ),

          tabPanel("📋 Summary Stats", br(),
            verbatimTextOutput("summary_stats")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # ---- Raw data (upload or built-in) ---------------------------------------
  raw_data <- reactive({
    if (!is.null(input$upload_csv)) {
      read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE)
    } else {
      read.csv("JC-202301-citibike-tripdata.csv", stringsAsFactors = FALSE)
    }
  })

  eng_data <- eventReactive(input$run_btn, {
    req(raw_data())
    df <- raw_data() %>% slice_sample(n = min(input$sample_n, nrow(raw_data())))
    withProgress(message = "Engineering features…", value = 0.5,
      engineer_features(df, input$selected_feats)
    )
  }, ignoreNULL = FALSE)

  observe({
    df <- eng_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "plot_feature", choices = num_cols,
                      selected = if ("trip_duration_min" %in% num_cols) "trip_duration_min" else num_cols[1])
    updateSelectInput(session, "scatter_x",    choices = num_cols,
                      selected = if ("trip_duration_min" %in% num_cols) "trip_duration_min" else num_cols[1])
    updateSelectInput(session, "scatter_y",    choices = num_cols,
                      selected = if ("haversine_distance_km" %in% num_cols) "haversine_distance_km"
                                 else num_cols[min(2, length(num_cols))])
  })

  output$feature_guide_ui <- renderUI({
    sel <- input$selected_feats
    if (length(sel) == 0) return(tags$p("No features selected yet.", class = "text-muted"))

    tagList(
      tags$p(tags$b(length(sel), "feature(s) will be added to the dataset:")),
      tags$table(
        class = "table table-striped table-sm table-hover",
        tags$thead(tags$tr(
          tags$th("New Column Name"),
          tags$th("Type"),
          tags$th("Description")
        )),
        tags$tbody(lapply(sel, function(f) {
          type_label <- if (f %in% numeric_features) "Numeric" else "Categorical"
          tags$tr(
            tags$td(tags$code(f)),
            tags$td(tags$span(type_label,
              class = if (type_label == "Numeric") "badge bg-info" else "badge bg-secondary")),
            tags$td(feature_descriptions[f])
          )
        }))
      ),
      tags$hr(),
      tags$p(tags$b("How to use:"), " Select features on the left, adjust the sample size,
        then click ", tags$b("▶ Apply Features"), ". Explore results in the other tabs.
        Download the engineered dataset using the button in the sidebar.")
    )
  })

  output$preview_table <- renderDT({
    req(eng_data())
    datatable(
      head(eng_data(), 300),
      options  = list(scrollX = TRUE, pageLength = 15,
                      dom = "lftip", autoWidth = TRUE),
      rownames = FALSE,
      filter   = "top"
    )
  })

  output$dist_plot <- renderPlot({
    df   <- eng_data()
    feat <- input$plot_feature
    grp  <- input$plot_group
    req(feat %in% names(df))

    cap  <- quantile(df[[feat]], 0.99, na.rm = TRUE)
    df2  <- df %>% filter(.data[[feat]] <= cap, !is.na(.data[[feat]]))

    has_group <- grp != "None" && grp %in% names(df2)

    p <- if (input$plot_type == "Boxplot") {
      if (has_group) {
        ggplot(df2, aes(x = .data[[grp]], y = .data[[feat]], fill = .data[[grp]])) +
          geom_boxplot(alpha = 0.75, outlier.size = 0.6) +
          theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
          labs(x = grp)
      } else {
        ggplot(df2, aes(y = .data[[feat]])) +
          geom_boxplot(fill = "steelblue", alpha = 0.75)
      }
    } else {
      base <- if (has_group)
        ggplot(df2, aes(x = .data[[feat]], fill = .data[[grp]], colour = .data[[grp]]))
      else
        ggplot(df2, aes(x = .data[[feat]]))

      if (input$plot_type == "Histogram")
        base + geom_histogram(bins = 40, alpha = 0.65, position = "identity")
      else
        base + geom_density(alpha = 0.45)
    }

    p + theme_minimal(base_size = 13) +
        labs(title = paste("Distribution of", feat),
             fill = grp, colour = grp) +
        theme(legend.position = "bottom",
              plot.title = element_text(face = "bold"))
  })

  output$dist_summary <- renderPrint({
    df   <- eng_data()
    feat <- input$plot_feature
    req(feat %in% names(df))
    cat("Summary of", feat, ":\n")
    print(summary(df[[feat]]))
  })

  output$scatter_plot <- renderPlot({
    df  <- eng_data()
    x   <- input$scatter_x
    y   <- input$scatter_y
    col <- input$scatter_color
    req(x %in% names(df), y %in% names(df))

    cap_x <- quantile(df[[x]], 0.99, na.rm = TRUE)
    cap_y <- quantile(df[[y]], 0.99, na.rm = TRUE)
    df2   <- df %>%
      filter(.data[[x]] <= cap_x, .data[[y]] <= cap_y,
             !is.na(.data[[x]]), !is.na(.data[[y]])) %>%
      slice_sample(n = min(3000, nrow(.)))

    p <- if (col != "None" && col %in% names(df2))
      ggplot(df2, aes(.data[[x]], .data[[y]], colour = .data[[col]]))
    else
      ggplot(df2, aes(.data[[x]], .data[[y]]))

    p + geom_point(alpha = 0.35, size = 1.3) +
        geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.8) +
        theme_minimal(base_size = 13) +
        labs(title = paste(y, "vs.", x),
             x = x, y = y,
             caption = "Capped at 99th percentile · max 3,000 points shown") +
        theme(plot.title = element_text(face = "bold"),
              legend.position = "bottom")
  })

  output$summary_stats <- renderPrint({
    df       <- eng_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) == 0) {
      cat("No numeric engineered features selected.\n")
    } else {
      df %>% select(all_of(num_cols)) %>% summary() %>% print()
    }
  })

  output$download_btn <- downloadHandler(
    filename = function() paste0("citibike_engineered_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(eng_data(), file, row.names = FALSE)
  )
}
shinyApp(ui = ui, server = server)
