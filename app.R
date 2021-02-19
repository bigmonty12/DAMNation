# DAM Shiny application for analyzing data recorded by Trikinetics DAM system
# Code developed by Austin Montgomery

# Load libraries

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(purrr)
library(plyr)
library(ggplot2)
library(dplyr)
library(zoo)
library(gtools)
library(scales)
library(gridExtra)
library(data.table)
library(matrixStats)
library(lubridate)
library(colourpicker)
library(grid)
library(janitor, quietly = TRUE)


# Define UI for the application
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "DAM Nation"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Preprocess Data", tabName = "preprocessing", icon = icon("warehouse")),
      menuItem("Preview Results", tabName = "analysis", icon = icon("table")),
      conditionalPanel(
        'input.go > 0',
        uiOutput("selectFiles"),
        downloadBttn(
          outputId = "downloadFiles",
          style = "bordered",
          color = "primary"
        )
      )
    )
  ),
  dashboardBody(
    tabItems(
      source("settings_ui.R", local = TRUE)$value,
      source("analysis_ui.R", local = TRUE)$value
    )
  )
)

server <- function(input, output, session){
  
  # Sets max file upload to 300 MB. Default is 5 MB.
  options(shiny.maxRequestSize=300*1024^2)
  
  source("settings_server.R", local = TRUE)$value
  source("analysis_server.R", local = TRUE)$value
  source("download_server.R", local = TRUE)$value
}
    
# Run the application 
shinyApp(ui = ui, server = server)
