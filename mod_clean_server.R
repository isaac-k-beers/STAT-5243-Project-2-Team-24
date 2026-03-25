library(shiny)
library(DT)
library(dplyr)
library(tidyr)

mod_clean_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    
    raw_data <- reactive({
      req(data_r())
      as.data.frame(data_r())
    })
    
    all_cols <- reactive({
      names(raw_data())
    })
    
    numeric_cols <- reactive({
      cols <- names(raw_data())
      cols[sapply(raw_data(), is.numeric)]
    })
    
    observe({
      req(raw_data())
      
      updateCheckboxGroupInput(session, "keep_cols",
                               choices = all_cols(), selected = character(0))
      updateCheckboxGroupInput(session, "datetime_cols",
                               choices = all_cols(), selected = character(0))
      updateCheckboxGroupInput(session, "factor_cols",
                               choices = all_cols(), selected = character(0))
      updateCheckboxGroupInput(session, "numeric_cols",
                               choices = all_cols(), selected = character(0))
      updateCheckboxGroupInput(session, "character_cols",
                               choices = all_cols(), selected = character(0))
      updateCheckboxGroupInput(session, "missing_cols",
                               choices = all_cols(), selected = character(0))
      updateCheckboxGroupInput(session, "scale_cols",
                               choices = numeric_cols(), selected = character(0))
      updateCheckboxGroupInput(session, "outlier_cols",
                               choices = numeric_cols(), selected = character(0))
    })
    
    observeEvent(input$keep_all, {
      updateCheckboxGroupInput(session, "keep_cols",
                               choices = all_cols(), selected = all_cols())
    })
    observeEvent(input$keep_none, {
      updateCheckboxGroupInput(session, "keep_cols",
                               choices = all_cols(), selected = character(0))
    })
    
    observeEvent(input$datetime_all, {
      updateCheckboxGroupInput(session, "datetime_cols",
                               choices = all_cols(), selected = all_cols())
    })
    observeEvent(input$datetime_none, {
      updateCheckboxGroupInput(session, "datetime_cols",
                               choices = all_cols(), selected = character(0))
    })
    
    observeEvent(input$factor_all, {
      updateCheckboxGroupInput(session, "factor_cols",
                               choices = all_cols(), selected = all_cols())
    })
    observeEvent(input$factor_none, {
      updateCheckboxGroupInput(session, "factor_cols",
                               choices = all_cols(), selected = character(0))
    })
    
    observeEvent(input$numeric_all, {
      updateCheckboxGroupInput(session, "numeric_cols",
                               choices = all_cols(), selected = all_cols())
    })
    observeEvent(input$numeric_none, {
      updateCheckboxGroupInput(session, "numeric_cols",
                               choices = all_cols(), selected = character(0))
    })
    
    observeEvent(input$character_all, {
      updateCheckboxGroupInput(session, "character_cols",
                               choices = all_cols(), selected = all_cols())
    })
    observeEvent(input$character_none, {
      updateCheckboxGroupInput(session, "character_cols",
                               choices = all_cols(), selected = character(0))
    })
    
    observeEvent(input$missing_all, {
      updateCheckboxGroupInput(session, "missing_cols",
                               choices = all_cols(), selected = all_cols())
    })
    observeEvent(input$missing_none, {
      updateCheckboxGroupInput(session, "missing_cols",
                               choices = all_cols(), selected = character(0))
    })
    
    observeEvent(input$scale_all, {
      updateCheckboxGroupInput(session, "scale_cols",
                               choices = numeric_cols(), selected = numeric_cols())
    })
    observeEvent(input$scale_none, {
      updateCheckboxGroupInput(session, "scale_cols",
                               choices = numeric_cols(), selected = character(0))
    })
    
    observeEvent(input$outlier_all, {
      updateCheckboxGroupInput(session, "outlier_cols",
                               choices = numeric_cols(), selected = numeric_cols())
    })
    observeEvent(input$outlier_none, {
      updateCheckboxGroupInput(session, "outlier_cols",
                               choices = numeric_cols(), selected = character(0))
    })
    
    cleaned_data <- reactive({
      req(raw_data())
      df <- raw_data()
      
      if (!is.null(input$keep_cols) && length(input$keep_cols) > 0) {
        df <- keep_selected_columns(df, input$keep_cols)
      }
      
      if (isTRUE(input$remove_dupes)) {
        df <- remove_duplicate_rows(df)
      }
      
      if (!is.null(input$datetime_cols) && length(input$datetime_cols) > 0) {
        df <- parse_selected_datetime_cols(df, input$datetime_cols)
      }
      
      df <- convert_selected_types(
        df,
        factor_cols = input$factor_cols,
        numeric_cols = input$numeric_cols,
        character_cols = input$character_cols
      )
      
      target_missing_cols <- if (!is.null(input$missing_cols) && length(input$missing_cols) > 0) {
        input$missing_cols
      } else {
        names(df)
      }
      
      df <- handle_missing_values(
        df,
        method = input$missing_method,
        selected_cols = target_missing_cols
      )
      
      if (isTRUE(input$remove_outliers) &&
          !is.null(input$outlier_cols) &&
          length(input$outlier_cols) > 0) {
        df <- remove_outliers_iqr(df, input$outlier_cols)
      }
      
      if (isTRUE(input$do_scale) &&
          !is.null(input$scale_cols) &&
          length(input$scale_cols) > 0) {
        df <- scale_selected_numeric(df, input$scale_cols)
      }
      
      df
    })
    
    output$overview_before <- renderTable({
      req(raw_data())
      get_data_overview(raw_data())
    })
    
    output$missing_before <- renderDT({
      req(raw_data())
      datatable(
        get_missing_summary(raw_data()),
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })
    
    output$overview_after <- renderTable({
      req(cleaned_data())
      get_data_overview(cleaned_data())
    })
    
    output$missing_after <- renderDT({
      req(cleaned_data())
      datatable(
        get_missing_summary(cleaned_data()),
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })
    
    output$clean_preview <- renderDT({
      req(cleaned_data())
      datatable(
        head(cleaned_data(), 100),
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    output$download_cleaned <- downloadHandler(
      filename = function() {
        paste0("cleaned_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(cleaned_data(), file, row.names = FALSE)
      }
    )
    
    return(list(
      data = cleaned_data
    ))
  })
}