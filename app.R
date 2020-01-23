# DAM Shiny application for analyzing data recorded by Trikinetics DAM system
# Code developed by Austin Montgomery

# Load libraries

library(shiny)
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
#library(Kmisc)
library(lubridate)
library(colourpicker)
library(grid)
library(janitor, quietly = TRUE)


# Define UI for the application
ui <- fluidPage(
  
  # This blocks printing any errors in the Shiny UI. These errors are mostly uninterpretable to a user anyway. 
  # Use with Caution, disable during development :) 
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  tabsetPanel(
    
    tabPanel(
      h4("Settings and Daily Locomotor Activity", style = "color: #2750D6;"),
      
      #Java script for Google Analytics - tracks the app usage recording the localization of the app launches.
      # tags$head(includeScript("google-analytics.js")),       
      
      source("settings_ui.R", local=TRUE)[1]
    )

  )
)



###################################   SERVER   ###################################

# Define server logic
server <- function(input, output) {
  
  # Sets max file upload to 300 MB. Default is 5 MB.
  options(shiny.maxRequestSize=300*1024^2)
  
  
  
  ### Initial Data pre-processing, including error messages ###
  source("settings_server.R", local = TRUE)   
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
