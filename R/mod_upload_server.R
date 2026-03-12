mod_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    error_message <- shiny::reactiveVal("")
    
    current_data <- shiny::reactive({
      error_message("")
      
      tryCatch({
        if (input$data_source == "builtin") {
          get_builtin_dataset(input$builtin_dataset)
          
        } else if (input$data_source == "upload") {
          req(input$file)
          
          ext <- tools::file_ext(input$file$name)
          read_uploaded_data(input$file$datapath, ext)
          
        } else {
          NULL
        }
      }, error = function(e) {
        error_message(e$message)
        NULL
      })
    })
    
    output$preview_table <- DT::renderDT({
      df <- current_data()
      req(df)
      
      DT::datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    output$metadata_table <- shiny::renderTable({
      df <- current_data()
      req(df)
      
      meta <- get_metadata(df)
      types <- get_column_types(df)
      
      rbind(
        meta,
        data.frame(
          Metric = paste("Type:", types$Column),
          Value = types$Type
        )
      )
    })
    
    output$summary_output <- shiny::renderPrint({
      df <- current_data()
      req(df)
      
      summary(df)
    })
    
    output$error_text <- shiny::renderText({
      error_message()
    })
    
    return(
      list(
        data = shiny::reactive(current_data())
      )
    )
    
  })
}