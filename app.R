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
library(survival)


# Add global variables
averageValues <- c("Day Total sleep", "Night Total sleep", "Total Total sleep",
                  "Day Mean sleep", "Night Mean sleep", "Total Mean sleep",
                  "Day Activity average", "Night Activity average", "Total Activity average",
                  "Day Activity rate", "Night Activity rate", "Total Activity rate")
dailyValues <- c("Total Sleep", "Sleep latency")
hourlyAverages <- c("Sleep Profile", "Activity Profile")

title <- tags$div(href="https://github.com/bigmonty12/DAMNation",
                tags$img(src="logo.png",
                         height = "60", width = "60"),
                "DAM Nation")

# Define UI for the application
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 60px}"),
            tags$style(".main-header .logo {height: 60px}"),
            tags$style(".sidebar-toggle {height: 60px; padding-top: 20px !important;}"),
            tags$style(".navbar {min-height:60px !important}")
    ),
    title = title),
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 60px}"),
    sidebarMenu(
      id = "tabs",
      menuItem("Preprocess Data", tabName = "preprocessing", icon = icon("warehouse")),
      menuItem("Preview Results", tabName = "analysis", icon = icon("table")),
      menuItem("Plot Results", tabName = "plots", icon = icon("chart-bar")),
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
    waiter::use_garcon(),
    waiter::use_waiter(),
    waiter::waiter_show_on_load(
      tags$img(
        src="logo.png",
        height=300,
        id = "myImage"
      )
    ),
    shinyjs::useShinyjs(),
    tabItems(
      source("settings_ui.R", local = TRUE)$value,
      source("analysis_ui.R", local = TRUE)$value,
      source("plots_ui.R", local = TRUE)$value
    )
  )
)

server <- function(input, output, session){
  
  # Sets max file upload to 300 MB. Default is 5 MB.
  options(shiny.maxRequestSize=300*1024^2)
  
  g <- waiter::Garcon$new("myImage", filter = "blur")
  waiter::waiter_hide()
  
  source("settings_server.R", local = TRUE)$value
  source("analysis_server.R", local = TRUE)$value
  source("plots_server.R", local = TRUE)$value
  source("download_server.R", local = TRUE)$value
}
    
# Run the application 
shinyApp(ui = ui, server = server)
