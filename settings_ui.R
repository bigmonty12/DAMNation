# settings_ui.R
# Settings tab of Shiny app

tabItem(tabName = "preprocessing",
        waiter::use_waiter(),
        shinyjs::useShinyjs(),
        h2("Settings"),
        fluidRow(
          box(
            fileInput("fin", "Upload Monitor Files", multiple = TRUE,
                      accept = c('text/csv', 'text/comma-separated-values,text/plain',
                                 '.csv')),
            width = 5
          ),
          box(
            numericInput("numConditions", "Number of Conditions", value = 1, min = 1, step = 1),
            uiOutput("conditions", inline = TRUE),
            width = 5
          )
        ),
        fluidRow(
          
        ),
        fluidRow(
          box(
            airDatepickerInput(
              inputId = "range",
              label = "Dates of experiment:",
              range = TRUE, value = c(Sys.Date()-3, Sys.Date()-1)
            ),
            width = 5
          ),
          box(
            numericInput('data_recording_frequency', 'DAM data bins [min]', value = 2, min = 1,
                         max = NA, step = 1, width = NULL),
            width = 3
          ),
          box(
            selectInput('light_onset_time', 'Light onset time',
                        c("00:00","01:00","02:00","03:00","04:00","05:00","06:00", 
                          "07:00", "08:00","09:00","10:00","11:00","12:00","13:00",
                          "14:00","15:00", "16:00","17:00","18:00","19:00","20:00",
                          "21:00","22:00","23:00"),
                        selected = '08:00', multiple = FALSE, selectize = TRUE),
            width = 3
          )
        ),
        fluidRow(
          box(
            verbatimTextOutput("statusText"),
            width = 12
          )
        ),
        fluidRow(
          column(
            3,
            actionBttn('status', 'Data Check', color = "primary", style = "pill"),
            actionBttn('go', 'Analyze Data', color = 'success', style = 'pill')
          )
        )
      )

#     wellPanel(
#       fluidRow(
#         column(7,
#                tags$h3("Sleep-o-gram"))
#       ),
#       fluidRow(
#         column(12,
#                plotOutput('ave_agg_sleep'))
#       ),
#       fluidRow(
#         column(3,
#                downloadButton("downloadSleepGram", "Download Sleep-o-gram")),
#         column(3,
#                downloadButton("downloadSleepGramData", "Download Sleep-o-gram Data"))
#       )
#     ),
#     wellPanel(
#       fluidRow(
#         column(7,
#                tags$h3("Act-o-gram"))
#       ),
#       fluidRow(
#         column(12,
#                plotOutput('activity_graph'))
#       ),
#       fluidRow(
#         column(3,
#                downloadButton("downloadActGram", "Download Act-o-gram")),
#         column(3,
#                downloadButton("downloadActGramData", "Download Act-o-gram Data"))
#       )
#     )
#     )
# )

  
