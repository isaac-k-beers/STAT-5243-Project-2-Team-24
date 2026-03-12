mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons(
          inputId = ns("data_source"),
          label = "Choose data source",
          choices = c("Upload File" = "upload",
                      "Built-in Dataset" = "builtin"),
          selected = "upload"
        ),
        
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("data_source")),
          shiny::fileInput(
            inputId = ns("file"),
            label = "Upload dataset",
            accept = c(".csv", ".xlsx", ".xls", ".json", ".rds")
          )
        ),
        
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'builtin'", ns("data_source")),
          shiny::selectInput(
            inputId = ns("builtin_dataset"),
            label = "Select built-in dataset",
            choices = c("iris", "mtcars", "sample_clean", "sample_messy")
          )
        ),
        
        shiny::h4("Dataset Metadata"),
        shiny::tableOutput(ns("metadata_table")),
        
        shiny::h4("Dataset Summary"),
        shiny::verbatimTextOutput(ns("summary_output")),
        
        shiny::br(),
        shiny::textOutput(ns("error_text"))
      ),
      
      shiny::mainPanel(
        shiny::h3("Data Preview"),
        DT::DTOutput(ns("preview_table"))
      )
    )
  )
}