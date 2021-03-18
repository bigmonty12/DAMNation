# plots_ui.R
# Plotting tab of Shiny app

tabItem(
  tabName = "plots",
  fluidRow(
    column(width = 3,
           box(
             title = "Plot Conditions", width = NULL, status = "primary",
             pickerInput("plotVariable", 'Variable to Plot',
                         choices = list(
                           averageValues = append(averageValues, "Time of Death"),
                           dailyValues = dailyValues,
                           hourlyAverages = hourlyAverages
                         )),
             uiOutput("colors", inline = TRUE),
             pickerInput('plotType', 'Plot Type', c('box', 'violin', 'bar', 'density', 'line', 'mortality')),
             radioButtons('addPoints', 'Add Data Points', c('Yes', 'No'), inline = TRUE),
             radioButtons('stderr', 'Display SEM bars', c('Yes', 'No'), inline = TRUE),
             actionBttn('plotButton', 'Create Plot', color = 'royal', style = 'material-flat',icon = icon("chart-area"))
           )),
    column(width = 9,
           conditionalPanel(
             'input.plotButton > 0',
             box(
               status = "primary", width = NULL,
               plotOutput("plot"),
               downloadBttn(
                 outputId = "downloadPlot",
                 label = "Download Plot",
                 style = "minimal",
                 color = "success"
               )
             )
           )
        )
  )
)