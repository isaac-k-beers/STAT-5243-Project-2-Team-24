source("R/utils_clean.R")
source("R/mod_clean_ui.R")
source("R/mod_clean_server.R")

library(shiny)
library(DT)
library(dplyr)
library(tidyr)

# ========= 直接读取你指定的 Citi Bike 数据文件 =========
sample_df <- read.csv(
  "/Users/qiuji/Downloads/5205 /5205 group/data(M3,6,9,12,2023Y)/JC-202303-citibike-tripdata.csv",
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  titlePanel("General Data Cleaning & Preprocessing App"),
  tabsetPanel(
    tabPanel("Cleaning", mod_clean_ui("clean"))
  )
)

server <- function(input, output, session) {
  sample_data <- reactive({
    sample_df
  })
  
  clean_res <- mod_clean_server(
    "clean",
    data_r = sample_data
  )
}

shinyApp(ui, server)