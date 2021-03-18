# plots_server.R
source("DAM_plotting.R")

observeEvent(input$plotVariable, {
  variable <- input$plotVariable
  choices = c('box', 'violin', 'bar', 'density', 'line', 'mortality')
  

  if (variable %in% averageValues){
    plotChoices <- c('box', 'violin', 'bar', 'density')
  } else if (variable %in% hourlyAverages){
    plotChoices <- c('line')
  } else if (variable == "Time of Death"){
    plotChoices <- 'mortality'
  } else if (variable %in% dailyValues){
    plotChoices <- c('box', 'violin', 'bar', 'line')
  }

  updatePickerInput(
    session = session, inputId = "plotType",
    choices = plotChoices
    )
})

output$colors <- renderUI({
  conditionNames <- getConditionNames()
  lapply(1:length(conditionNames), function(i){
    name <- conditionNames[[i]]
    colourpicker::colourInput(
      inputId = paste0("color", i),
      label = paste("Color:", name),
      value = "black",
      allowTransparent = TRUE,
      closeOnClick = TRUE
    )
  })
})

getConditionColors <- reactive({
  numConditions <- findNumConditions()
  colors <- list()
  colors <- lapply(1:numConditions, function(i) {
    colors[[ i ]] <- input[[paste0('color', i)]]
  })
  colors
})

mergeAverages <- reactive({
  aveSums <- averagesSummarizer()
  combined <- rbind.fill(aveSums)
  melt <- reshape2::melt(combined, id=1)
})

mergeDaily <- reactive({
  dailySums <- dailySummarizer()
  combined <- rbind.fill(dailySums)
  melt <- reshape2::melt(combined, id = 1)
})

mergeAveSleep <- reactive({
  aveSleep <- aveSleepDT()
  combined <- rbind.fill(aveSleep)
  melt <- reshape2::melt(combined, id = c(1,2))
  melt$variable <- NULL
  melt$Hour <- as.integer(melt$Hour)
  melt
})

mergeAveActivity <- reactive({
  aveActivity <- formatActivity()
  combined <- rbind.fill(aveActivity)
  melt <- reshape2::melt(combined, id = c(1,2))
  melt$variable <- NULL
  melt$Hour <- as.integer(melt$Hour)
  melt
})

filterData <- reactive({
  chosenVariable <- input$plotVariable
  
  
  mergeAverages <- mergeAverages()
  mergeDaily <- mergeDaily()
  mergeAveSleep <- mergeAveSleep()
  mergeAveActivity <- mergeAveActivity()
  
  # Filter appropriate dataset for chosen variable
  if (chosenVariable == "Total Sleep"){
    df <- mergeDaily %>% filter(stringr::str_detect(variable, "Total Sleep"))
  } else if (chosenVariable == "Sleep latency"){
    df <- mergeDaily %>% filter(stringr::str_detect(variable, "latency"))
  } else if (chosenVariable == "Sleep Profile"){
    df <- mergeAveSleep
  } else if (chosenVariable == "Activity Profile"){
    df <- mergeAveActivity
  }else {
    df <- mergeAverages %>% filter(stringr::str_detect(variable, chosenVariable))
  }
  
  if (chosenVariable != "Time of Death"){
    # Remove dead flies and make value column numeric
    df <- df %>% tidyr::drop_na()
    df$value <- as.numeric(df$value)
    
    # Create columns with number of observations, mean, sd, and se
    if (chosenVariable %in% averageValues){
      sample_size <- df %>%
        group_by(Condition.Genotype) %>% 
        summarize(num=n(), mean=mean(value), sd=sd(value)) %>%
        mutate(se=sd/sqrt(num))
    } else if (chosenVariable %in% dailyValues) {
      sample_size <- df %>%
        group_by(Condition.Genotype, variable) %>% 
        summarize(num=n(), mean=mean(value), sd=sd(value)) %>%
        mutate(se=sd/sqrt(num))
    } else {
      sample_size <- df %>%
        group_by(Condition.Genotype, Hour) %>% 
        summarize(num=n(), mean=mean(value), sd=sd(value)) %>%
        mutate(se=sd/sqrt(num))
    }
    df <- df %>% left_join(sample_size) %>%
      mutate(myaxis = paste0(Condition.Genotype, " (", "n=", num, ")"))
  } else {
    date1 <- paste(dateRange()[1], "00:00:00")
    df <- df %>%
      mutate(days = as.double(difftime(strptime(value, format = "%Y-%m-%d %H:%M:%S"),
                                       strptime(date1, format = "%Y-%m-%d %H:%M:%S"),
                                       units = "days"))) %>%
      mutate(dead = !is.na(value))
    
  }
  df
})

output$activityList <- DT::renderDataTable({
  combined <- filterData()
  DT::datatable(combined)
})

createPlot <- reactive({
  chosenVariable <- input$plotVariable
  plotType <- input$plotType
  addPoints <- input$addPoints
  se <- input$stderr
  colors <- unlist(getConditionColors())
  names <- unlist(getConditionNames())

  dates <- dateRange()
  dates <- dates[-c(length(dates))]

  df <- filterData()
  
  if (chosenVariable %in% averageValues){
    plot <- plotAverageSummary(df, colors, plotType, addPoints, se)
  } else if (chosenVariable %in% dailyValues){
    plot <- plotDailySummary(df, colors, chosenVariable, plotType, dates, addPoints, se)
  } else if (chosenVariable == "Time of Death"){
    plot <- plotMortality(df, colors, names)
  } else if (chosenVariable == "Sleep Profile"){
    plot <- plotAveSleep(df, colors, se)
  } else {
    plot <- plotAveActivity(df, colors, se)
  }
  
  plot
})

plotLoadingScreen <- tagList(
  h3("Plotting data...", style = "color:white;"),
  img(src="logo.png", height = "300")
)
plotWaiter <- waiter::Waiter$new(id="plot", html = plotLoadingScreen)
output$plot <- renderPlot({
  plotButton()
  plotWaiter$show()
  plot <- isolate(createPlot())
  plot
})

plotButton <- eventReactive(input$plotButton, {
  inFile()
})