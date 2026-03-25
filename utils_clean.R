library(dplyr)

get_data_overview <- function(df) {
  data.frame(
    metric = c("Rows", "Columns", "Duplicate Rows"),
    value = c(nrow(df), ncol(df), sum(duplicated(df)))
  )
}

get_missing_summary <- function(df) {
  data.frame(
    column = names(df),
    type = sapply(df, function(x) class(x)[1]),
    missing_count = sapply(df, function(x) sum(is.na(x))),
    missing_pct = round(sapply(df, function(x) mean(is.na(x))) * 100, 2),
    row.names = NULL
  )
}

safe_parse_datetime_vector <- function(x) {
  x_chr <- as.character(x)
  
  formats <- c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%Y-%m-%d",
    "%m/%d/%Y"
  )
  
  parsed <- rep(as.POSIXct(NA, tz = "UTC"), length(x_chr))
  
  for (fmt in formats) {
    idx <- which(is.na(parsed) & !is.na(x_chr) & trimws(x_chr) != "")
    if (length(idx) == 0) break
    
    attempt <- as.POSIXct(x_chr[idx], format = fmt, tz = "UTC")
    parsed[idx] <- attempt
  }
  
  parsed
}

parse_selected_datetime_cols <- function(df, cols) {
  valid_cols <- cols[cols %in% names(df)]
  
  for (col in valid_cols) {
    parsed <- tryCatch(
      safe_parse_datetime_vector(df[[col]]),
      error = function(e) rep(as.POSIXct(NA, tz = "UTC"), length(df[[col]]))
    )
    
    # 只有当这一列至少有一些值能成功解析，才替换原列
    if (sum(!is.na(parsed)) > 0) {
      df[[col]] <- parsed
    }
  }
  
  df
}

convert_selected_types <- function(df, factor_cols = NULL, numeric_cols = NULL, character_cols = NULL) {
  if (!is.null(factor_cols)) {
    valid_factor <- factor_cols[factor_cols %in% names(df)]
    for (col in valid_factor) df[[col]] <- as.factor(df[[col]])
  }
  
  if (!is.null(numeric_cols)) {
    valid_numeric <- numeric_cols[numeric_cols %in% names(df)]
    for (col in valid_numeric) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  
  if (!is.null(character_cols)) {
    valid_character <- character_cols[character_cols %in% names(df)]
    for (col in valid_character) df[[col]] <- as.character(df[[col]])
  }
  
  df
}

handle_missing_values <- function(df, method = "none", selected_cols = NULL) {
  if (method == "none") return(df)
  
  target_cols <- if (is.null(selected_cols) || length(selected_cols) == 0) {
    names(df)
  } else {
    selected_cols[selected_cols %in% names(df)]
  }
  
  if (length(target_cols) == 0) return(df)
  
  if (method == "drop_rows") {
    return(df %>% tidyr::drop_na(dplyr::all_of(target_cols)))
  }
  
  if (method == "median_mode") {
    for (col in target_cols) {
      if (is.numeric(df[[col]])) {
        med <- median(df[[col]], na.rm = TRUE)
        if (is.finite(med)) df[[col]][is.na(df[[col]])] <- med
      } else if (is.character(df[[col]])) {
        df[[col]][is.na(df[[col]]) | trimws(df[[col]]) == ""] <- "Unknown"
      } else if (is.factor(df[[col]])) {
        if (!("Unknown" %in% levels(df[[col]]))) {
          levels(df[[col]]) <- c(levels(df[[col]]), "Unknown")
        }
        df[[col]][is.na(df[[col]])] <- "Unknown"
      }
    }
  }
  
  df
}

remove_duplicate_rows <- function(df) {
  df[!duplicated(df), , drop = FALSE]
}

keep_selected_columns <- function(df, cols) {
  valid_cols <- cols[cols %in% names(df)]
  if (length(valid_cols) == 0) return(df)
  df[, valid_cols, drop = FALSE]
}

scale_selected_numeric <- function(df, cols) {
  valid_cols <- cols[cols %in% names(df)]
  valid_cols <- valid_cols[sapply(df[valid_cols], is.numeric)]
  if (length(valid_cols) == 0) return(df)
  
  for (col in valid_cols) {
    if (sd(df[[col]], na.rm = TRUE) > 0) {
      df[[col]] <- as.numeric(scale(df[[col]]))
    }
  }
  
  df
}

remove_outliers_iqr <- function(df, cols) {
  valid_cols <- cols[cols %in% names(df)]
  valid_cols <- valid_cols[sapply(df[valid_cols], is.numeric)]
  if (length(valid_cols) == 0) return(df)
  
  keep <- rep(TRUE, nrow(df))
  
  for (col in valid_cols) {
    q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    iqr_val <- q3 - q1
    
    if (is.na(iqr_val) || iqr_val == 0) next
    
    lower <- q1 - 1.5 * iqr_val
    upper <- q3 + 1.5 * iqr_val
    
    keep <- keep & (is.na(df[[col]]) | (df[[col]] >= lower & df[[col]] <= upper))
  }
  
  df[keep, , drop = FALSE]
}
