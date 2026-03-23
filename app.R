library(shiny)
library(DT)
library(readr)
library(readxl)
library(jsonlite)

source("R/utils_io.R")
source("R/mod_upload_ui.R")
source("R/mod_upload_server.R")

ui <- fluidPage(
  titlePanel("Loading Datasets Test"),
  mod_upload_ui("upload")
)

server <- function(input, output, session) {
  
  upload <- mod_upload_server("upload")
  
}

shinyApp(ui, server)