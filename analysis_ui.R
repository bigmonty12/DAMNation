# analysis_ui.R
# Analysis/Preview tab of Shiny app

tabItem(
  tabName = "analysis",
  waiter::use_waiter(),
  shinyjs::useShinyjs(),
  uiOutput("previews")
)