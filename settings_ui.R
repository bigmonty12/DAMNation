# settings.R
# Home page of Shiny app

fluidPage(
  fluidRow(
    column(12,
           tags$h1("DAM Nation", align = "center"),
           tags$h3("Program analyzing locomotor activity in Trikinetics DAM system",
                   align = "center"),
           tags$h4("Austin Montgomery, Rothenfluh Lab, University of Utah",
                   align = "center"),
           
           fileInput('file1', 'Upload Monitor Files', multiple = TRUE,
                     accept = c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv')
           )
    )
  ),
  
  wellPanel(
    fluidRow(
      column(2,
             dateInput('dateStart',
                            label = 'Start Date',
                            format = "d M yy",
                            value = Sys.Date()-10)),
      column(2,
             numericInput('experimentLength',
                          'Number of Days',
                          value = 3,
                          min = 1,
                          max = NA,
                          step = 1)),
      column(3,
             numericInput("data_recording_frequency", 
                          "DAM system data bins [min]",
                          value = 2, 
                          min = 1, 
                          max = NA, 
                          step = 1, 
                          width = NULL)),
      column(3,
             selectInput("light_onset_time", "Light onset time",
                         c("00:00","01:00","02:00","03:00","04:00","05:00","06:00", 
                           "07:00", "08:00","09:00","10:00","11:00","12:00","13:00",
                           "14:00","15:00", "16:00","17:00","18:00","19:00","20:00",
                           "21:00","22:00","23:00"), 
                         selected = "08:00", 
                         multiple = FALSE,
                         selectize = TRUE, 
                         width = NULL, 
                         size = NULL)),
      column(2,
             numericInput('monitorNumber',
                          'Monitor Number',
                          value = 1,
                          min = 1,
                          max = NA,
                          step = 1))
    )
  ),
  
  fluidRow(
    column(2,
           tags$head(
             tags$style(HTML('#go{background-color:#67EB5E}'))),
           actionButton("go", "Start Analysis"))),
    # column(1,
    #        tags$head(
    #          tags$style(HTML('#reset{background-color:#F22E2E'))
    #        ),
    #        actionButton("reset", "Reset"))),
  
  # ==== Output Plots ====
  conditionalPanel(
    condition = "input.go != 0",
    
    wellPanel(
      fluidRow(
        column(7,
               tags$h3("DAM Summary"))),
      fluidRow(
        column(12,
               tableOutput('summary'))
      ),
      fluidRow(
        column(3,
               downloadButton("downloadSummary", "Download DAM-Summary"))
      )
      ),
    wellPanel(
      fluidRow(
        column(7,
               tags$h3("DAM Bouts"))),
      fluidRow(
        column(12,
               tableOutput('bouts'))
      ),
      fluidRow(
        column(3,
               downloadButton("downloadBouts", "Download DAM-Bouts"))
      )
    ),
    wellPanel(
      fluidRow(
        column(7,
               tags$h3("DAM Average Sleep"))),
      fluidRow(
        column(12,
               tableOutput('ave_sleep'))
      ),
      fluidRow(
        column(3,
               downloadButton("downloadAveSleep", "Download DAM-Average-Sleep"))
      )
      ),
    wellPanel(
      fluidRow(
        column(7,
               tags$h3("Sleep-o-gram"))
      ),
      fluidRow(
        column(12,
               plotOutput('ave_agg_sleep'))
      ),
      fluidRow(
        column(3,
               downloadButton("downloadSleepGram", "Download Sleep-o-gram")),
        column(3,
               downloadButton("downloadSleepGramData", "Download Sleep-o-gram Data"))
      )
    ),
    wellPanel(
      fluidRow(
        column(7,
               tags$h3("Act-o-gram"))
      ),
      fluidRow(
        column(12,
               plotOutput('activity_graph'))
      ),
      fluidRow(
        column(3,
               downloadButton("downloadActGram", "Download Act-o-gram")),
        column(3,
               downloadButton("downloadActGramData", "Download Act-o-gram Data"))
      )
    )
    )
)

  
