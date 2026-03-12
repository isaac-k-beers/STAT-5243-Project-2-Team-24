get_builtin_dataset <- function(name) {
  if (name == "iris") {
    return(iris)
  } else if (name == "mtcars") {
    return(mtcars)
  } else if (name == "sample_clean") {
    return(readr::read_csv("data/sample_clean.csv", show_col_types = FALSE))
  } else if (name == "sample_messy") {
    return(readr::read_csv("data/sample_messy.csv", show_col_types = FALSE))
  } else {
    stop("Unsupported built-in dataset.")
  }
}

read_uploaded_data <- function(path, ext) {
  ext <- tolower(ext)
  
  if (ext == "csv") {
    return(readr::read_csv(path, show_col_types = FALSE))
    
  } else if (ext == "xlsx" || ext == "xls") {
    return(readxl::read_excel(path))
    
  } else if (ext == "json") {
    data <- jsonlite::fromJSON(path)
    
    if (!is.data.frame(data)) {
      stop("JSON file must contain tabular data.")
    }
    
    return(data)
    
  } else if (ext == "rds") {
    data <- readRDS(path)
    
    if (!is.data.frame(data)) {
      stop("RDS must contain a data frame.")
    }
    
    return(data)
    
  } else {
    stop("Unsupported file type. Please upload CSV, Excel, JSON, or RDS.")
  }
}

get_metadata <- function(df) {
  data.frame(
    Metric = c("Rows", "Columns"),
    Value = c(nrow(df), ncol(df))
  )
}

get_column_types <- function(df) {
  data.frame(
    Column = names(df),
    Type = sapply(df, function(x) class(x)[1])
  )
}