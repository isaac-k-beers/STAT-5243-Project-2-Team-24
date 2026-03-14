# =========================
# Universal Data Preprocessing App (P1 + P2)
# =========================

options(shiny.maxRequestSize = 1000 * 1024^2)

attached_pkgs <- c("shiny", "DT", "dplyr", "readr", "readxl")
namespace_pkgs <- c("jsonlite")
all_required_pkgs <- c(attached_pkgs, namespace_pkgs)

missing_required <- all_required_pkgs[
  !vapply(all_required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_required) > 0) {
  install.packages(missing_required, repos = "https://cloud.r-project.org")
}

for (p in attached_pkgs) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required but not installed.")
}

if (!requireNamespace("arrow", quietly = TRUE)) {
  message("Optional package 'arrow' is not installed, so .parquet files will not be supported.")
}

clean_names_base <- function(x) {
  x <- trimws(x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  blank_idx <- which(x == "" | is.na(x))
  if (length(blank_idx) > 0) x[blank_idx] <- paste0("col_", blank_idx)
  make.unique(x, sep = "_")
}

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
  if (inherits(obj, "data.frame")) {
    return(as.data.frame(obj, stringsAsFactors = FALSE))
  }
  if (is.matrix(obj)) {
    return(as.data.frame(obj, stringsAsFactors = FALSE))
  }
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
    return(data.frame(line_id = seq_along(lines), text = lines, stringsAsFactors = FALSE))
  }

  out <- tryCatch(
    readr::read_delim(path, delim = delim, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )

  if (is.null(out)) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    data.frame(line_id = seq_along(lines), text = lines, stringsAsFactors = FALSE)
  } else {
    as.data.frame(out, stringsAsFactors = FALSE)
  }
}

read_any_file <- function(path, ext) {
  ext <- tolower(ext)
  df <- switch(
    ext,
    "csv" = as.data.frame(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), stringsAsFactors = FALSE),
    "tsv" = as.data.frame(readr::read_tsv(path, show_col_types = FALSE, progress = FALSE), stringsAsFactors = FALSE),
    "txt" = read_txt_as_df(path),
    "json" = read_json_as_df(path),
    "xlsx" = as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE),
    "rds" = coerce_to_df(readRDS(path)),
    "parquet" = {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop("Package 'arrow' is required for parquet files.")
      }
      as.data.frame(arrow::read_parquet(path), stringsAsFactors = FALSE)
    },
    stop(paste("Unsupported file type:", ext))
  )
  flatten_list_cols(df)
}

replace_blank_with_na <- function(df) {
  for (nm in names(df)) {
    if (is.character(df[[nm]])) {
      x <- trimws(df[[nm]])
      x[x == ""] <- NA_character_
      df[[nm]] <- x
    }
  }
  df
}

parse_date_time <- function(x) {
  x0 <- trimws(x)
  fmts <- c(
    "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
    "%m/%d/%Y %H:%M:%S", "%d/%m/%Y %H:%M:%S",
    "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M",
    "%m/%d/%Y %H:%M", "%d/%m/%Y %H:%M",
    "%Y-%m-%d", "%Y/%m/%d",
    "%m/%d/%Y", "%d/%m/%Y"
  )
  idx <- which(!is.na(x0) & nzchar(x0))
  if (length(idx) == 0) return(NULL)

  best <- NULL
  best_score <- -1
  for (fmt in fmts) {
    parsed <- if (grepl("%H", fmt)) {
      as.POSIXct(x0, format = fmt, tz = "UTC")
    } else {
      as.Date(x0, format = fmt)
    }
    score <- mean(!is.na(parsed[idx]))
    if (score > best_score) {
      best <- parsed
      best_score <- score
    }
  }
  if (best_score >= 0.9) best else NULL
}

infer_semantic_type <- function(x) {
  if (inherits(x, "POSIXt")) return("datetime")
  if (inherits(x, "Date")) return("date")
  if (is.numeric(x)) return("numeric")
  if (is.integer(x)) return("integer")
  if (is.logical(x)) return("logical")
  if (is.factor(x)) return("categorical")
  if (is.list(x)) return("nested_or_list")

  if (is.character(x)) {
    xx <- trimws(x)
    nonmiss <- xx[!is.na(xx) & nzchar(xx)]
    if (length(nonmiss) == 0) return("empty_text")

    num_share <- mean(!is.na(suppressWarnings(as.numeric(nonmiss))))
    bool_share <- mean(tolower(nonmiss) %in% c("true", "false", "t", "f", "yes", "no", "y", "n", "0", "1"))
    date_try <- parse_date_time(nonmiss)
    uniq_ratio <- length(unique(nonmiss)) / length(nonmiss)

    if (num_share >= 0.9) return("numeric_like_text")
    if (!is.null(date_try)) {
      if (inherits(date_try, "POSIXt")) return("datetime_like_text")
      return("date_like_text")
    }
    if (bool_share >= 0.9) return("logical_like_text")
    if (length(unique(nonmiss)) <= 20 || uniq_ratio <= 0.2) return("categorical_text")
    return("free_text")
  }

  paste(class(x), collapse = "/")
}

profile_df <- function(df) {
  data.frame(
    column = names(df),
    storage_class = vapply(df, function(x) paste(class(x), collapse = "/"), character(1)),
    semantic_type = vapply(df, infer_semantic_type, character(1)),
    missing_n = vapply(df, function(x) sum(is.na(x)), integer(1)),
    missing_pct = round(vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)), 2),
    unique_n = vapply(df, function(x) dplyr::n_distinct(x, na.rm = TRUE), integer(1)),
    sample_values = vapply(df, function(x) {
      vals <- unique(x[!is.na(x)])
      vals <- vals[seq_len(min(length(vals), 3))]
      paste(as.character(vals), collapse = " | ")
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

quality_summary <- function(df) {
  nrows <- nrow(df)
  ncols <- ncol(df)
  dup_rows <- if (nrows == 0) 0 else sum(duplicated(df))
  total_missing <- sum(is.na(df))
  all_missing_cols <- sum(vapply(df, function(x) all(is.na(x)), logical(1)))
  constant_cols <- sum(vapply(df, function(x) dplyr::n_distinct(x, na.rm = TRUE) <= 1, logical(1)))

  text_cols <- names(df)[vapply(df, is.character, logical(1))]
  blank_cells <- if (length(text_cols) == 0) 0 else {
    sum(vapply(df[text_cols], function(x) sum(trimws(x) == "", na.rm = TRUE), integer(1)))
  }

  data.frame(
    metric = c("rows", "columns", "duplicate_rows", "missing_cells", "blank_text_cells", "all_missing_columns", "constant_columns"),
    value = c(nrows, ncols, dup_rows, total_missing, blank_cells, all_missing_cols, constant_cols),
    stringsAsFactors = FALSE
  )
}

auto_convert_text_cols <- function(df, log_vec) {
  for (nm in names(df)) {
    x <- df[[nm]]
    if (!is.character(x)) next

    xtype <- infer_semantic_type(x)

    if (xtype == "numeric_like_text") {
      df[[nm]] <- suppressWarnings(as.numeric(x))
      log_vec <- c(log_vec, paste0("Auto-converted '", nm, "' from text to numeric."))
    } else if (xtype %in% c("date_like_text", "datetime_like_text")) {
      parsed <- parse_date_time(x)
      if (!is.null(parsed)) {
        df[[nm]] <- parsed
        log_vec <- c(log_vec, paste0("Auto-converted '", nm, "' from text to ", ifelse(inherits(parsed, "POSIXt"), "datetime", "date"), "."))
      }
    } else if (xtype == "logical_like_text") {
      low <- tolower(trimws(x))
      out <- rep(NA, length(low))
      out[low %in% c("true", "t", "yes", "y", "1")] <- TRUE
      out[low %in% c("false", "f", "no", "n", "0")] <- FALSE
      df[[nm]] <- out
      log_vec <- c(log_vec, paste0("Auto-converted '", nm, "' from text to logical."))
    } else if (xtype == "categorical_text") {
      df[[nm]] <- as.factor(x)
      log_vec <- c(log_vec, paste0("Converted '", nm, "' to factor (categorical)."))
    }
  }
  list(df = df, log = log_vec)
}

clip_iqr <- function(x) {
  if (!is.numeric(x)) return(x)
  q1 <- quantile(x, 0.25, na.rm = TRUE, type = 7)
  q3 <- quantile(x, 0.75, na.rm = TRUE, type = 7)
  iqr <- q3 - q1
  low <- q1 - 1.5 * iqr
  high <- q3 + 1.5 * iqr
  pmin(pmax(x, low), high)
}

winsor_1_99 <- function(x) {
  if (!is.numeric(x)) return(x)
  q <- quantile(x, c(0.01, 0.99), na.rm = TRUE, type = 7)
  pmin(pmax(x, q[1]), q[2])
}

scale_minmax <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (!is.finite(r[1]) || r[1] == r[2]) return(x)
  (x - r[1]) / (r[2] - r[1])
}

preprocess_df <- function(df, opt) {
  log_vec <- character(0)
  out <- df

  if (isTRUE(opt$std_names)) {
    old_names <- names(out)
    names(out) <- clean_names_base(names(out))
    if (!identical(old_names, names(out))) log_vec <- c(log_vec, "Standardized column names.")
  }

  if (isTRUE(opt$trim_ws) && any(vapply(out, is.character, logical(1)))) {
    for (nm in names(out)) {
      if (is.character(out[[nm]])) out[[nm]] <- trimws(out[[nm]])
    }
    log_vec <- c(log_vec, "Trimmed whitespace in character columns.")
  }

  if (isTRUE(opt$blank_to_na)) {
    out <- replace_blank_with_na(out)
    log_vec <- c(log_vec, "Converted blank text cells to NA.")
  }

  if (isTRUE(opt$lower_text) && any(vapply(out, is.character, logical(1)))) {
    for (nm in names(out)) {
      if (is.character(out[[nm]])) out[[nm]] <- tolower(out[[nm]])
    }
    log_vec <- c(log_vec, "Lowercased character columns.")
  }

  if (isTRUE(opt$auto_convert)) {
    res <- auto_convert_text_cols(out, log_vec)
    out <- res$df
    log_vec <- res$log
  }

  if (isTRUE(opt$remove_dups)) {
    before <- nrow(out)
    out <- unique(out)
    log_vec <- c(log_vec, paste0("Removed ", before - nrow(out), " duplicate row(s)."))
  }

  if (!is.null(opt$missing_method) && opt$missing_method != "none") {
    if (opt$missing_method == "drop_any_na") {
      before <- nrow(out)
      out <- out[complete.cases(out), , drop = FALSE]
      log_vec <- c(log_vec, paste0("Dropped ", before - nrow(out), " row(s) containing NA."))
    } else {
      for (nm in names(out)) {
        x <- out[[nm]]
        if (is.numeric(x)) {
          if (opt$missing_method == "numeric_median_text_missing") {
            fill <- median(x, na.rm = TRUE)
            if (!is.na(fill)) out[[nm]][is.na(x)] <- fill
          } else if (opt$missing_method == "numeric_mean_text_missing") {
            fill <- mean(x, na.rm = TRUE)
            if (!is.na(fill)) out[[nm]][is.na(x)] <- fill
          } else if (opt$missing_method == "zero_and_missing_label") {
            out[[nm]][is.na(x)] <- 0
          }
        } else if (is.factor(x)) {
          tmp <- as.character(x)
          tmp[is.na(tmp)] <- "Missing"
          out[[nm]] <- factor(tmp)
        } else if (is.character(x)) {
          x[is.na(x)] <- "Missing"
          out[[nm]] <- x
        }
      }
      log_vec <- c(log_vec, paste0("Applied missing-value strategy: ", opt$missing_method, "."))
    }
  }

  if (!is.null(opt$outlier_method) && opt$outlier_method != "none") {
    num_cols <- names(out)[vapply(out, is.numeric, logical(1))]
    for (nm in num_cols) {
      out[[nm]] <- if (opt$outlier_method == "clip_iqr") clip_iqr(out[[nm]]) else winsor_1_99(out[[nm]])
    }
    if (length(num_cols) > 0) log_vec <- c(log_vec, paste0("Applied outlier handling (", opt$outlier_method, ") to numeric columns."))
  }

  if (!is.null(opt$scale_method) && opt$scale_method != "none") {
    num_cols <- names(out)[vapply(out, is.numeric, logical(1))]
    for (nm in num_cols) {
      if (opt$scale_method == "zscore") {
        s <- sd(out[[nm]], na.rm = TRUE)
        if (is.finite(s) && s > 0) out[[nm]] <- as.numeric(scale(out[[nm]]))
      } else {
        out[[nm]] <- scale_minmax(out[[nm]])
      }
    }
    if (length(num_cols) > 0) log_vec <- c(log_vec, paste0("Scaled numeric columns using ", opt$scale_method, "."))
  }

  attr(out, "log") <- unique(log_vec)
  out
}

prepare_for_json <- function(df) {
  out <- df
  for (nm in names(out)) {
    if (inherits(out[[nm]], "Date") || inherits(out[[nm]], "POSIXt")) {
      out[[nm]] <- as.character(out[[nm]])
    } else if (is.factor(out[[nm]])) {
      out[[nm]] <- as.character(out[[nm]])
    }
  }
  out
}

ui <- shiny::fluidPage(
  shiny::titlePanel("Universal Data Preprocessing App (P1 + P2)"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(
        "file", "Upload data file",
        accept = c(".csv", ".tsv", ".txt", ".json", ".xlsx", ".rds", ".parquet")
      ),
      shiny::tags$div(
        style = "margin-top: 8px; margin-bottom: 12px; font-size: 13px; color: #666666; background-color: #f8f9fa; padding: 8px 10px; border-radius: 6px;",
        shiny::tags$p(
          shiny::tags$b("Supported formats: "),
          "CSV, TSV, TXT, JSON, XLSX, RDS, and Parquet.",
          style = "margin-bottom: 4px;"
        ),
        shiny::tags$p(
          shiny::tags$b("Example input: "),
          "If you do not have your own file, please use the ",
          shiny::tags$a(
            "sample Citi Bike dataset (CSV format)",
            href = "JC-202301-citibike-tripdata.csv",
            target = "_blank",
            download = "JC-202301-citibike-tripdata.csv"
          ),
          ".",
          style = "margin-bottom: 0;"
        )
      ),
      shiny::tags$hr(),
      shiny::checkboxInput("std_names", "Standardize column names", TRUE),
      shiny::checkboxInput("trim_ws", "Trim whitespace", TRUE),
      shiny::checkboxInput("blank_to_na", "Convert blank text to NA", TRUE),
      shiny::checkboxInput("lower_text", "Lowercase character columns", FALSE),
      shiny::checkboxInput("auto_convert", "Auto-convert numeric/date/logical-like text", TRUE),
      shiny::checkboxInput("remove_dups", "Remove duplicate rows", TRUE),
      shiny::selectInput(
        "missing_method", "Missing values",
        choices = c("none", "drop_any_na", "numeric_median_text_missing", "numeric_mean_text_missing", "zero_and_missing_label"),
        selected = "numeric_median_text_missing"
      ),
      shiny::selectInput(
        "outlier_method", "Outlier handling",
        choices = c("none", "clip_iqr", "winsor_1_99"),
        selected = "none"
      ),
      shiny::selectInput(
        "scale_method", "Numeric scaling",
        choices = c("none", "zscore", "minmax"),
        selected = "none"
      ),
      shiny::tags$hr(),
      shiny::downloadButton("download_csv", "Download cleaned CSV"),
      shiny::br(), shiny::br(),
      shiny::downloadButton("download_json", "Download cleaned JSON")
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(
          "P1 Upload & Audit",
          shiny::br(),
          shiny::h4("Raw preview"),
          DT::DTOutput("raw_preview"),
          shiny::br(),
          shiny::h4("Schema / type profile"),
          DT::DTOutput("schema_tbl"),
          shiny::br(),
          shiny::h4("Quality summary"),
          DT::DTOutput("quality_tbl"),
          shiny::br(),
          shiny::h4("Read message"),
          shiny::verbatimTextOutput("read_msg")
        ),
        shiny::tabPanel(
          "P2 Preprocess",
          shiny::br(),
          shiny::h4("Processed preview"),
          DT::DTOutput("clean_preview"),
          shiny::br(),
          shiny::h4("Processed schema"),
          DT::DTOutput("clean_schema_tbl"),
          shiny::br(),
          shiny::h4("Transformation log"),
          shiny::verbatimTextOutput("transform_log")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  raw_df <- shiny::reactive({
    shiny::req(input$file)
    ext <- tools::file_ext(input$file$name)

    df <- tryCatch(read_any_file(input$file$datapath, ext), error = function(e) e)

    if (inherits(df, "error")) {
      stop(paste("Failed to read file:", conditionMessage(df)))
    }
    if (!is.data.frame(df)) {
      stop("Uploaded object could not be converted into a data frame.")
    }
    if (ncol(df) <= 0) {
      stop("The uploaded data has 0 columns after parsing.")
    }
    df
  })

  raw_schema <- shiny::reactive({
    shiny::req(raw_df())
    profile_df(raw_df())
  })

  raw_quality <- shiny::reactive({
    shiny::req(raw_df())
    quality_summary(raw_df())
  })

  clean_df <- shiny::reactive({
    shiny::req(raw_df())
    opt <- list(
      std_names = input$std_names,
      trim_ws = input$trim_ws,
      blank_to_na = input$blank_to_na,
      lower_text = input$lower_text,
      auto_convert = input$auto_convert,
      remove_dups = input$remove_dups,
      missing_method = input$missing_method,
      outlier_method = input$outlier_method,
      scale_method = input$scale_method
    )
    preprocess_df(raw_df(), opt)
  })

  clean_schema <- shiny::reactive({
    shiny::req(clean_df())
    profile_df(clean_df())
  })

  output$raw_preview <- DT::renderDT({
    shiny::req(raw_df())
    DT::datatable(head(raw_df(), 100), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$schema_tbl <- DT::renderDT({
    shiny::req(raw_schema())
    DT::datatable(raw_schema(), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$quality_tbl <- DT::renderDT({
    shiny::req(raw_quality())
    DT::datatable(raw_quality(), options = list(dom = "t", scrollX = TRUE))
  })

  output$read_msg <- shiny::renderText({
    shiny::req(input$file, raw_df())
    ext <- tolower(tools::file_ext(input$file$name))
    paste0(
      "File name: ", input$file$name, "\n",
      "Detected extension: ", ext, "\n",
      "Rows: ", nrow(raw_df()), "\n",
      "Columns: ", ncol(raw_df()), "\n\n",
      if (ext == "txt") {
        "Note: if the .txt file is not tabular, it is loaded as line_id + text."
      } else {
        "Note: nested/list JSON columns are converted to JSON strings if needed."
      }
    )
  })

  output$clean_preview <- DT::renderDT({
    shiny::req(clean_df())
    DT::datatable(head(clean_df(), 100), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$clean_schema_tbl <- DT::renderDT({
    shiny::req(clean_schema())
    DT::datatable(clean_schema(), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$transform_log <- shiny::renderText({
    shiny::req(clean_df())
    logs <- attr(clean_df(), "log")
    if (is.null(logs) || length(logs) == 0) {
      "No transformation was applied."
    } else {
      paste(logs, collapse = "\n")
    }
  })

  output$download_csv <- shiny::downloadHandler(
    filename = function() paste0("cleaned_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(clean_df(), file, row.names = FALSE, na = "")
  )

  output$download_json <- shiny::downloadHandler(
    filename = function() paste0("cleaned_data_", Sys.Date(), ".json"),
    content = function(file) {
      out <- prepare_for_json(clean_df())
      jsonlite::write_json(out, file, pretty = TRUE, auto_unbox = TRUE, na = "null")
    }
  )
}

shiny::shinyApp(ui, server)
