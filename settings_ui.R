# settings_ui.R
# Settings tab of Shiny app

tabItem(tabName = "preprocessing",
        h2("Settings"),
        fluidRow(
          box(
            fileInput("fin", "Upload Monitor Files", multiple = TRUE,
                      accept = c('text/csv', 'text/comma-separated-values,text/plain',
                                 '.csv')),
            width = 5
          ),
          box(
            numericInput("numConditions", "Number of Conditions/Genotypes", value = 1, min = 1, step = 1),
            uiOutput("conditions", inline = TRUE),
            width = 5
          )
        ),
        fluidRow(
          box(
            airDatepickerInput(
              inputId = "range",
              label = "Dates of experiment (last day should be last desired full day of data)",
              range = TRUE, value = c(Sys.Date()-3, Sys.Date()-1)
            ),
            width = 5
          ),
          box(
            numericInput('data_recording_frequency', 'DAM data bins [min]', value = 2, min = 1,
                         max = 3, step = 1, width = NULL),
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
          conditionalPanel(
            'input.status > 0',
            box(
              verbatimTextOutput("statusText"),
              width = 12
            )
          )
        ),
        fluidRow(
          column(
            4,
            actionBttn('status', 'Data Check', color = "danger", style = "material-flat", icon = icon("list")),
            actionBttn('go', 'Analyze Data', color = 'success', style = 'material-flat', icon = icon("play"))
          )
        )
      )
